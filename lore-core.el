;;; lore-core.el --- Core pipeline for Lore -*- lexical-binding: t; -*-

;;; Commentary:
;; Parse -> Plan -> Run -> Merge -> Rank; Getter registry.

;;; Code:

(require 'cl-lib)
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
    (dolist (t tokens)
      (cond
       ((string-match-p "\\`\\?k=[0-9]+\\'" t)
        (setq k (string-to-number (substring t 3))))
       ((string= t "?scope=global") (setq scope 'global))
       ((string= t "?scope=project") (setq scope 'project))
       (t (push t rest))))
    (list (nreverse rest) k scope)))

(defun lore-parse-query (s &key at-point)
  "Parse query string S into request alist."
  (let* ((tokens (lore--tokenize (or s "")))
         (filters '())
         (targets '())
         (keywords '()))
    (dolist (t tokens)
      (cond
       ((string-match "\\`\\([a-zA-Z]+\\):\\(.*\\)" t)
        (let ((dom (intern (downcase (match-string 1 t))))
              (rest (match-string 2 t)))
          (push dom targets)
          (unless (string-empty-p rest)
            (push rest keywords))))
       ((string-prefix-p "#" t)
        (push (cons :tag (substring t 1)) filters))
       (t (push t keywords))))
    (cl-multiple-value-bind (tokens2 k scope)
        (apply #'values (lore--parse-flags keywords))
      (let ((req `((:query . ,s)
                   (:keywords . ,(mapcar #'downcase tokens2))
                   (:intent . search)
                   (:filters . ,filters)
                   (:targets . ,(delete-dups targets))
                   (:scope . ,scope)
                   (:max-k . ,k)
                   (:at-point . ,at-point)
                   (:ctx . nil)))))
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
  "Simple eligibility: if :targets non-empty, match :domains."
  (let* ((caps (plist-get getter :capabilities))
         (domains (plist-get caps :domains))
         (targets (alist-get :targets req)))
    (if (null targets) t
      (cl-some (lambda (tgt) (memq tgt domains)) targets))))

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
  "Run PLAN synchronously; return list of lore-result."
  (let* ((calls (plist-get plan :getters))
         (results (mapcar (lambda (c)
                            (let ((fn (plist-get c :fn))
                                  (args (plist-get c :args)))
                              (condition-case err
                                  (let ((out (apply fn args)))
                                    (when (and (listp out) (null (null out)))
                                      out))
                                (error
                                 (lore-log-warn "getter %S error: %S"
                                                (plist-get c :name) err)
                                 nil))))
                          calls))
         (flat (lore--aggregate results))
         (uniq (lore-uniq flat))
         (norm (lore-normalize-scores uniq))
         (ranked (lore-rank norm)))
    ranked))

(defun lore-run-async (plan callback)
  "Run PLAN asynchronously. CALLBACK called with (:done RESULTS)."
  (let ((token (lore--gen-token)))
    (lore-events-publish :lore-query-start (plist-get plan :request) token)
    (puthash token
             (run-at-time 0 nil
                          (lambda ()
                            (let ((res (lore-run plan)))
                              (remhash token lore--active-tasks)
                              (lore-events-publish :lore-done token res)
                              (when callback
                                (funcall callback :done res)))))
             lore--active-tasks)
    token))

(defun lore-cancel (token)
  "Cancel running task TOKEN."
  (let ((tm (gethash token lore--active-tasks)))
    (when (timerp tm)
      (cancel-timer tm)
      (remhash token lore--active-tasks)
      (lore-events-publish :lore-cancel token)
      t)))

(provide 'lore-core)
;;; lore-core.el ends here
