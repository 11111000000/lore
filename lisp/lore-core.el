;;; lore-core.el --- Core pipeline for Lore -*- lexical-binding: t; -*-

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

(cl-defun lore-parse-query (s &key at-point)
  "Parse query string S into request alist."
  (let* ((tokens (lore--tokenize (or s "")))
         (filters '())
         (targets '())
         (keywords '()))
    (dolist (tok tokens)
      (cond
       ((string-match "\\`\\([a-zA-Z]+\\):\\(.*\\)" tok)
        (let ((dom (intern (downcase (match-string 1 tok))))
              (rest (match-string 2 tok)))
          (push dom targets)
          (unless (string-empty-p rest)
            (push rest keywords))))
       ((string-prefix-p "#" tok)
        (push (cons :tag (substring tok 1)) filters))
       (t (push tok keywords))))
    (cl-destructuring-bind (tokens2 k scope)
        (lore--parse-flags keywords)
      (let ((req `((:query . ,s)
                   (:keywords . ,(mapcar #'downcase tokens2))
                   (:intent . search)
                   (:filters . ,filters)
                   (:targets . ,(delete-dups targets))
                   (:scope . ,scope)
                   (:max-k . ,k)
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
     ;; Scope match if declared
     (or (null scopes) (memq scope scopes))
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
         (calls (plist-get plan :getters)))
    (when (and (boundp 'lore-cache-enabled) lore-cache-enabled)
      (when-let ((cached (lore-cache-get key)))
        (lore-log-info "cache hit (sync) %s" key)
        (cl-return-from lore-run cached)))
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
           (norm (lore-normalize-scores uniq))
           (ranked (lore-rank norm)))
      (when (and (boundp 'lore-cache-enabled) lore-cache-enabled)
        (lore-cache-put key ranked lore-cache-ttl)
        (lore-log-info "cache store (sync) %s (%d)" key (length ranked)))
      ranked)))

(defun lore-run-async (plan callback)
  "Run PLAN asynchronously.
CALLBACK is called as (CALLBACK :partial RESULTS) and (CALLBACK :done RESULTS).
If a cached result is available, it is returned immediately and no token is created."
  (let* ((request (plist-get plan :request))
         (topk (alist-get :max-k request))
         (key (lore-request-fingerprint request))
         (maybe-cached (and (boundp 'lore-cache-enabled) lore-cache-enabled
                            (lore-cache-get key))))
    (when maybe-cached
      (let* ((res (if (and topk (> (length maybe-cached) topk))
                      (cl-subseq maybe-cached 0 topk)
                    maybe-cached)))
        (lore-log-info "cache hit (async) %s" key)
        (lore-events-publish :lore-query-start request nil)
        (lore-events-publish :lore-done nil res)
        (when callback (funcall callback :done res))
        (cl-return-from lore-run-async nil)))
    (let* ((token (lore--gen-token))
           (calls (plist-get plan :getters))
           (start (float-time))
           (state (list :token token
                        :request request
                        :acc nil
                        :pending 0
                        :cancels nil
                        :callback callback)))
      (lore-events-publish :lore-query-start request token)
      ;; Helper to merge, uniq, rank and deliver
      (cl-labels
          ((aggregate!
             (new)
             (let* ((acc (append (plist-get state :acc) new))
                    (uniq (lore-uniq acc))
                    (norm (lore-normalize-scores uniq))
                    (ranked (lore-rank norm))
                    (trimmed (if (and topk (> (length ranked) topk))
                                 (cl-subseq ranked 0 topk)
                               ranked)))
               (plist-put state :acc trimmed)
               trimmed))
           (emit!
             (batch)
             (when (and batch (listp batch))
               (let ((res (aggregate! batch)))
                 (lore-events-publish :lore-partial token res)
                 (when callback (funcall callback :partial res)))))
           (finalize!
             ()
             (let* ((res (or (plist-get state :acc) '()))
                    (dt  (- (float-time) start)))
               (remhash token lore--active-tasks)
               (when (and (boundp 'lore-cache-enabled) lore-cache-enabled)
                 (lore-cache-put key res lore-cache-ttl)
                 (lore-log-info "cache store (async) %s (%d)" key (length res)))
               (lore-log-info "lore-run-async done in %.3fs (%d results)" dt (length res))
               (lore-events-publish :lore-done token res)
               (when callback (funcall callback :done res)))))
        ;; Launch getters with parallel limit
        (let* ((queue (copy-sequence calls))
               (limit (max 1 (or lore-parallel-limit 1))))
          (plist-put state :pending 0)
          (plist-put state :queue queue)
          (cl-labels
              ((start-call!
                 (call)
                 (let* ((fn   (plist-get call :fn))
                        (args (plist-get call :args))
                        (emit (lambda (batch) (emit! batch)))
                        (done (lambda (&optional err)
                                (when err
                                  (lore-events-publish :lore-error token
                                                       (list :getter (plist-get call :name)
                                                             :error err)))
                                (let ((p (max 0 (1- (plist-get state :pending)))))
                                  (plist-put state :pending p))
                                ;; Start next from queue if any
                                (when (plist-get state :queue)
                                  (start-next!))
                                ;; If nothing pending and queue empty, finalize
                                (when (and (zerop (plist-get state :pending))
                                           (null (plist-get state :queue)))
                                  (finalize!)))))
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
                           (emit! out))))
                     (error
                      (lore-log-warn "getter %S error: %S" (plist-get call :name) err)))))
               (start-next!
                 ()
                 (let ((q (plist-get state :queue)))
                   (when q
                     (let ((next (car q)))
                       (plist-put state :queue (cdr q))
                       (start-call! next))))))
            ;; Kick off up to LIMIT calls
            (dotimes (_i (min limit (length queue)))
              (start-next!))))
        ;; Save state
        (puthash token state lore--active-tasks)
        ;; If no async pending and queue empty and nothing emitted yet, finalize immediately
        (when (and (zerop (plist-get state :pending))
                   (null (plist-get state :queue)))
          (finalize!))
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
