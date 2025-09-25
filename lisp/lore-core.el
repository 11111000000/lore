;;; lore-core.el --- Core pipeline for Lore -*- lexical-binding: t; -*-
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://github.com/11111000000/lore.el
;; SPDX-License-Identifier: MIT
;; Package-Requires: ((emacs "27.1"))
;; Keywords: core, pipeline, tools

;;; Commentary:
;; Parse -> Plan -> Run -> Merge -> Rank; Getter registry.

;;; Code:

(eval-and-compile
  (add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name))))

(require 'cl-lib)
(require 'subr-x)
(require 'lore-log)
(require 'lore-model)
(require 'lore-events)
(require 'lore-cache)

(defgroup lore-core nil
  "Core settings of Lore."
  :group 'lore)

(defcustom lore-max-k 20
  "Default top-K results per request."
  :type 'integer :group 'lore-core)

(defcustom lore-default-scope 'project
  "Default search scope."
  :type '(choice (const project) (const global)) :group 'lore-core)

(defcustom lore-parallel-limit 4
  "Max parallel async getters."
  :type 'integer :group 'lore-core)

(defcustom lore-source-weights
  '((elisp . 1.0)
    (grep  . 1.0)
    (org   . 1.0)
    (info  . 1.0)
    (man   . 1.0))
  "Weights to multiply raw scores by source before normalization.
Alist of (SOURCE . WEIGHT). Unknown sources default to 1.0."
  :type '(alist :key-type symbol :value-type number)
  :group 'lore-core)

(defun lore--apply-source-weights (results)
  "Return a new list of RESULTS with score adjusted by source weights.
Weights come from `lore-source-weights'."
  (let ((tbl (let ((h (make-hash-table :test 'eq)))
               (dolist (kv lore-source-weights)
                 (puthash (car kv) (cdr kv) h))
               h)))
    (mapcar (lambda (r)
              (let* ((src (lore-result-source r))
                     (w   (or (and src (gethash src tbl)) 1.0))
                     (nr  (copy-sequence r)))
                (setf (lore-result-score nr)
                      (* (or (lore-result-score r) 0.0) (float w)))
                nr))
            results)))

;; Getter registry

(defvar lore--getters (make-hash-table :test 'eq)
  "Getter registry: name -> plist (:capabilities :fn :cost :batch-p).")

(cl-defun lore-register-getter (name &key capabilities fn cost batch-p)
  "Register getter NAME with CAPABILITIES, FN, COST, BATCH-P."
  (puthash name (list :name name
                      :capabilities (or capabilities '())
                      :fn fn
                      :cost (or cost 1.0)
                      :batch-p (and batch-p t))
           lore--getters)
  (lore-log-info "registered getter %S" name)
  t)

(defun lore-unregister-getter (name)
  "Unregister getter NAME."
  (remhash name lore--getters))

(defun lore-getters ()
  "Return list of registered getter plists."
  (let (out) (maphash (lambda (_ v) (push v out)) lore--getters) out))

;; Parse and normalize

(defun lore--tokenize (s)
  (if (string-empty-p (or s ""))
      '()
    (split-string s "[ \t\n]+" t)))

(defun lore--parse-flags (tokens)
  (let ((k lore-max-k)
        (scope lore-default-scope)
        rest)
    (dolist (tok tokens)
      (cond
       ((string-match-p "\\`\\?k=[0-9]+\\'" tok)
        (setq k (string-to-number (substring tok 3))))
       ((string= tok "?scope=global") (setq scope 'global))
       ((string= tok "?scope=project") (setq scope 'project))
       (t (push tok rest))))
    (list (nreverse rest) k scope)))

(defun lore--parse-domain-token (tok)
  "Parse TOK like \"dom?flags:rest\" into plist (:domain SYM :flags STR|nil :rest STR)."
  (when (string-match "\\`\\([A-Za-z]+\\)\\(\\?[^:]+\\)?:\\(.*\\)\\'" tok)
    (list :domain (intern (downcase (match-string 1 tok)))
          :flags  (match-string 2 tok)  ;; includes leading ? when present
          :rest   (match-string 3 tok))))

(defun lore--parse-inline-flags (flagstr)
  "Parse FLAGSTR like \"?k=10?scope=global\" into plist (:k INT :scope SYM).
FLAGSTR may be nil; returns plist keys present only when parsed."
  (let ((out nil))
    (when (and flagstr (string-prefix-p "?" flagstr))
      ;; split by ? and parse known flags
      (dolist (frag (split-string (substring flagstr 1) "\\?" t))
        (cond
         ((string-match "\\`k=\\([0-9]+\\)\\'" frag)
          (setq out (plist-put out :k (string-to-number (match-string 1 frag)))))
         ((string-match "\\`scope=\\(project\\|global\\)\\'" frag)
          (setq out (plist-put out :scope (intern (match-string 1 frag))))))))
    out))

(cl-defun lore-parse-query (s &key at-point)
  "Parse query string S into request alist.
Supports inline flags embedded in domain tokens, e.g. \"man?scope=global: socket\"."
  (let* ((tokens (lore--tokenize (or s "")))
         (filters '())
         (targets '())
         (keywords '())
         (inline-k nil)
         (inline-scope nil))
    (dolist (tok tokens)
      (let ((domrec (lore--parse-domain-token tok)))
        (cond
         ;; Domain with optional inline flags: dom[?flags]:rest
         (domrec
          (let* ((dom (plist-get domrec :domain))
                 (flags (plist-get domrec :flags))
                 (rest (plist-get domrec :rest))
                 (pfl (lore--parse-inline-flags flags)))
            (push dom targets)
            (unless (string-empty-p rest)
              (push rest keywords))
            (when (plist-member pfl :k) (setq inline-k (plist-get pfl :k)))
            (when (plist-member pfl :scope) (setq inline-scope (plist-get pfl :scope)))))
         ;; Plain domain prefix: dom:rest
         ((string-match "\\`\\([a-zA-Z]+\\):\\(.*\\)" tok)
          (let ((dom (intern (downcase (match-string 1 tok))))
                (rest (match-string 2 tok)))
            (push dom targets)
            (unless (string-empty-p rest)
              (push rest keywords))))
         ;; Filters (#tag)
         ((string-prefix-p "#" tok)
          (push (cons :tag (substring tok 1)) filters))
         ;; Regular keyword
         (t (push tok keywords)))))
    ;; Preserve original token order
    (setq keywords (nreverse keywords)
          filters  (nreverse filters)
          targets  (nreverse targets))
    (cl-destructuring-bind (tokens2 k scope)
        (lore--parse-flags keywords)
      ;; Inline flags from domain tokens take precedence if provided
      (let* ((final-k (or inline-k k))
             (final-scope (or inline-scope scope))
             (req `((:query . ,s)
                    (:keywords . ,tokens2)
                    (:intent . search)
                    (:filters . ,filters)
                    (:targets . ,(delete-dups targets))
                    (:scope . ,final-scope)
                    (:max-k . ,final-k)
                    (:at-point . ,at-point)
                    (:ctx . nil))))
        req))))

(defun lore-normalize-request (req)
  "Normalize REQ alist ensuring required keys exist."
  (let ((defaults '((:keywords . ())
                    (:intent . search)
                    (:filters . ())
                    (:targets . ())
                    (:scope . project)
                    (:max-k . 20)
                    (:ctx . nil))))
    (dolist (kv defaults req)
      (unless (assq (car kv) req)
        (push kv req)))))

;; Planning

(defun lore--eligible-getter-p (getter req)
  "Decide if GETTER is eligible for REQ based on :targets and :scope."
  (let* ((caps (plist-get getter :capabilities))
         (domains (plist-get caps :domains))
         (scopes (plist-get caps :scope))
         (targets (alist-get :targets req))
         (scope   (alist-get :scope req)))
    (and
     ;; Scope match:
     ;; - no scopes declared => eligible
     ;; - exact scope match => eligible
     ;; - global-only getter is also OK for project requests
     (or (null scopes)
         (memq scope scopes)
         (and (eq scope 'project) (memq 'global scopes)))
     ;; Domains match if targets provided
     (or (null targets)
         (and domains (cl-some (lambda (tgt) (memq tgt domains)) targets))))))

(defun lore-plan (request)
  "Build plan for REQUEST (alist)."
  (let* ((req (lore-normalize-request request))
         (all (lore-getters))
         (eligible (cl-remove-if-not (lambda (g) (lore--eligible-getter-p g req)) all))
         (sorted (cl-sort (copy-sequence eligible) #'< :key (lambda (g) (or (plist-get g :cost) 1.0))))
         (topk (alist-get :max-k req))
         (calls (mapcar (lambda (g)
                          (list :name (plist-get g :name)
                                :fn (plist-get g :fn)
                                :args (list :request req :topk topk)
                                :cost (plist-get g :cost)))
                        sorted)))
    (lore-log-info "plan: scope=%s targets=%S getters=%S"
                   (alist-get :scope req)
                   (alist-get :targets req)
                   (mapcar (lambda (c) (plist-get c :name)) calls))
    (list :request req :getters calls)))

;; Running

(defvar lore--active-tasks (make-hash-table :test 'equal))

(defun lore--gen-token ()
  (format "lore-%x-%x" (random most-positive-fixnum) (float-time)))

(defun lore--aggregate (lists)
  (apply #'append (cl-remove-if-not #'identity lists)))

(defun lore-run (plan)
  "Run PLAN synchronously; return list of lore-result.
Respects cache; ignores async getters (those returning plist with :async)."
  (let* ((req (plist-get plan :request))
         (key (lore-request-fingerprint req))
         (calls (plist-get plan :getters))
         (cached (and (boundp 'lore-cache-enabled) lore-cache-enabled
                      (lore-cache-get key))))
    (if cached
        (progn
          (lore-log-info "cache hit (sync) %s" key)
          cached)
      (let* ((results (mapcar (lambda (c)
                                (let ((fn (plist-get c :fn))
                                      (args (plist-get c :args)))
                                  (condition-case err
                                      (let ((out (apply fn args)))
                                        (cond
                                         ;; Async marker: ignore in sync run
                                         ((and (listp out) (plist-get out :async)) nil)
                                         ;; List of results
                                         ((listp out) out)
                                         (t nil)))
                                    (error
                                     (lore-log-warn "getter %S error: %S"
                                                    (plist-get c :name) err)
                                     nil))))
                              calls))
             (flat (lore--aggregate results))
             (uniq (lore-uniq flat))
             (weighted (lore--apply-source-weights uniq))
             (norm (lore-normalize-scores weighted))
             (ranked (lore-rank norm)))
        (when (and (boundp 'lore-cache-enabled) lore-cache-enabled)
          (lore-cache-put key ranked lore-cache-ttl)
          (lore-log-info "cache store (sync) %s (%d)" key (length ranked)))
        ranked))))

;; Async helpers

(defun lore--trim-topk (results topk)
  "Trim RESULTS to TOPK if needed."
  (if (and topk (> (length results) topk))
      (cl-subseq results 0 topk)
    results))

(defun lore--aggregate-into-state (state new topk)
  "Aggregate NEW results into STATE and keep only TOPK ranked items."
  (let* ((acc (append (plist-get state :acc) new))
         (uniq (lore-uniq acc))
         (weighted (lore--apply-source-weights uniq))
         (norm (lore-normalize-scores weighted))
         (ranked (lore-rank norm))
         (trimmed (lore--trim-topk ranked topk)))
    (plist-put state :acc trimmed)
    trimmed))

(defun lore--emit-partial (state token batch topk callback)
  "Aggregate BATCH into STATE, publish partial events and call CALLBACK."
  (when (and batch (listp batch))
    (let ((res (lore--aggregate-into-state state batch topk)))
      (lore-events-publish :lore-partial token res)
      (when callback (funcall callback :partial res)))))

(defun lore--finalize-async (state key start token callback)
  "Finalize async run: cache, log, events, callback."
  (let* ((res (or (plist-get state :acc) '()))
         (dt  (- (float-time) start)))
    (remhash token lore--active-tasks)
    (when (and (boundp 'lore-cache-enabled) lore-cache-enabled)
      (lore-cache-put key res lore-cache-ttl)
      (lore-log-info "cache store (async) %s (%d)" key (length res)))
    (lore-log-info "lore-run-async done in %.3fs (%d results)" dt (length res))
    (lore-events-publish :lore-done token res)
    (when callback (funcall callback :done res))))

(defun lore--start-one-call (state call token topk key start callback)
  "Start a single GETTER CALL, wiring emit/done into STATE."
  (let* ((name (plist-get call :name))
         (fn   (plist-get call :fn))
         (args (plist-get call :args))
         (emit (lambda (batch) (lore--emit-partial state token batch topk callback)))
         (done (lambda (&optional err)
                 (when err
                   (lore-events-publish :lore-error token
                                        (list :getter name
                                              :error err)))
                 (let ((p (max 0 (1- (plist-get state :pending)))))
                   (plist-put state :pending p))
                 ;; Start next from queue if any
                 (when (plist-get state :queue)
                   (lore--async-start-next state token topk key start callback))
                 ;; If nothing pending and queue empty, finalize
                 (when (and (zerop (plist-get state :pending))
                            (null (plist-get state :queue)))
                   (lore--finalize-async state key start token callback)))))
    (lore-log-info "async: start getter=%S token=%s" name token)
    (condition-case err
        (let ((out (apply fn (append args (list :emit emit :done done)))))
          (cond
           ;; Async getter: register cancel and pending
           ((and (listp out) (plist-get out :async))
            (plist-put state :pending (1+ (plist-get state :pending)))
            (let ((cancel (plist-get out :cancel)))
              (when cancel
                (plist-put state :cancels (cons cancel (plist-get state :cancels))))))
           ;; Sync list: aggregate immediately
           ((listp out)
            (funcall emit out))))
      (error
       (lore-log-warn "getter %S error: %S" name err)))))

(defun lore--async-start-next (state token topk key start callback)
  "Pop next call from STATE queue and start it."
  (let ((q (plist-get state :queue)))
    (when q
      (let ((next (car q)))
        (plist-put state :queue (cdr q))
        (lore--start-one-call state next token topk key start callback)))))

(defun lore--maybe-return-cached-async (request key topk callback)
  "If cache has RESULT for REQUEST KEY, emit events/callback and return t."
  (let ((maybe-cached (and (boundp 'lore-cache-enabled) lore-cache-enabled
                           (lore-cache-get key))))
    (when maybe-cached
      (let* ((res (lore--trim-topk maybe-cached topk)))
        (lore-log-info "cache hit (async) %s" key)
        (lore-events-publish :lore-query-start request nil)
        (lore-events-publish :lore-done nil res)
        (when callback (funcall callback :done res))
        t))))

(defun lore-run-async (plan callback)
  "Run PLAN asynchronously.
CALLBACK is called as (CALLBACK :partial RESULTS) and (CALLBACK :done RESULTS).
If a cached result is available, it is returned immediately and no token is created."
  (let* ((request (plist-get plan :request))
         (topk (alist-get :max-k request))
         (key (lore-request-fingerprint request)))
    (if (lore--maybe-return-cached-async request key topk callback)
        nil
      (let* ((token (lore--gen-token))
             (calls (plist-get plan :getters))
             (start (float-time))
             (state (list :token token
                          :request request
                          :acc nil
                          :pending 0
                          :cancels nil
                          :callback callback)))
        (lore-log-info "async: start token=%s getters=%d topk=%s" token (length calls) topk)
        (lore-events-publish :lore-query-start request token)
        ;; Launch getters with parallel limit
        (let* ((queue (copy-sequence calls))
               (limit (max 1 (or lore-parallel-limit 1))))
          (plist-put state :pending 0)
          (plist-put state :queue queue)
          ;; Kick off up to LIMIT calls
          (dotimes (_i (min limit (length queue)))
            (lore--async-start-next state token topk key start callback)))
        ;; Save state
        (puthash token state lore--active-tasks)
        ;; If no async pending and queue empty and nothing emitted yet, finalize immediately
        (when (and (zerop (plist-get state :pending))
                   (null (plist-get state :queue)))
          (lore--finalize-async state key start token callback))
        token))))

(defun lore-cancel (token)
  "Cancel running task TOKEN."
  (let ((st (gethash token lore--active-tasks)))
    (when st
      (dolist (c (plist-get st :cancels))
        (condition-case _err
            (when (functionp c) (funcall c))
          (error nil)))
      (remhash token lore--active-tasks)
      (lore-events-publish :lore-cancel token)
      t)))

(provide 'lore-core)
;;; lore-core.el ends here
