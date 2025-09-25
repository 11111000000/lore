;;; lore-getter-info.el --- Info apropos getter for Lore.el  -*- lexical-binding: t; -*-
;; Author: Lore.el
;; Keywords: info, docs, tools
;; Package-Requires: ((emacs "27.1"))
;; URL: https://example.invalid/lore
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Asynchronous getter using external `info --apropos' to search Info manuals.
;; - Streams partial results via emit; finalizes with done.
;; - Produces lore-result of type 'doc with meta (:file :node :desc).
;;
;; Contract:
;;   (lore-getter-info-run &key request topk emit done)
;;
;; Parsing strategy:
;;   We try to match common GNU info apropos output patterns, e.g.
;;     (emacs) Buffers: Node about buffers...
;;     (libc) printf: Formatted output...
;;   Fallbacks are handled gracefully.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'lore-model nil t)
(require 'lore-core  nil t)

(defgroup lore-getter-info nil
  "Info-based getter for Lore.el."
  :group 'lore)

(defcustom lore-info-program "info"
  "Program used to access GNU Info."
  :type 'string
  :group 'lore-getter-info)

(defcustom lore-info-extra-args '("--apropos")
  "Extra arguments for `info'."
  :type '(repeat string)
  :group 'lore-getter-info)

(defvar lore-getter-info--state (make-hash-table :test #'eq)
  "Internal state: process → plist(:buf BUF :emit FN :done FN :limit INT :emitted INT :acc LIST).")

(defvar lore-getter-info--tok 0
  "Monotonic token counter for info getter.")

(defun lore-getter-info--pattern (keywords)
  "Build regex pattern for KEYWORDS (join with \".*\")."
  (cond
   ((null keywords) "")
   ((= (length keywords) 1) (car keywords))
   (t (mapconcat #'identity keywords ".*"))))

(defun lore-getter-info--parse-line (line)
  "Parse info apropos LINE. Return (FILE NODE DESC) or nil."
  ;; Try patterns like "(manual) Node: desc"
  (cond
   ((string-match "\\`(\\([^()]+\\))\\s-*\\([^:]+\\):\\s-*\\(.*\\)\\'" line)
    (list (string-trim (match-string 1 line))
          (string-trim (match-string 2 line))
          (string-trim (match-string 3 line))))
   ;; Some distros output: "manual: Node: desc" (without parens)
   ((string-match "\\`\\([^:]+\\):\\s-*\\([^:]+\\):\\s-*\\(.*\\)\\'" line)
    (list (string-trim (match-string 1 line))
          (string-trim (match-string 2 line))
          (string-trim (match-string 3 line))))
   (t nil)))

(defun lore-getter-info--make-result (file node desc score)
  "Create lore-result for FILE manual, NODE, DESC with SCORE."
  (if (fboundp 'lore-result-create)
      (lore-result-create
       :type 'doc
       :title (format "(%s) %s" file node)
       :snippet desc
       :content nil
       :path nil
       :url nil
       :buffer nil
       :beg nil :end nil
       :score (float score)
       :source 'info
       :meta (list :file file :node node :desc desc))
    `((type . doc)
      (title . ,(format "(%s) %s" file node))
      (snippet . ,desc)
      (content . nil)
      (path . nil)
      (url . nil)
      (buffer . nil)
      (beg . nil) (end . nil)
      (score . ,(float score))
      (source . info)
      (meta . ,(list :file file :node node :desc desc)))))

(defun lore-getter-info--emit-batch (proc)
  "Emit accumulated results for PROC."
  (let* ((st (gethash proc lore-getter-info--state))
         (emit (plist-get st :emit))
         (acc (plist-get st :acc)))
    (when (and (functionp emit) acc)
      (funcall emit (nreverse acc))
      (plist-put st :acc nil)
      (puthash proc st lore-getter-info--state))))

(defun lore-getter-info--filter (proc chunk)
  "Process filter for PROC handling CHUNK."
  (let ((st (gethash proc lore-getter-info--state)))
    (when st
      (let* ((buf (plist-get st :buf))
             (limit (or (plist-get st :limit) most-positive-fixnum))
             (emit (plist-get st :emit))
             (emitted (or (plist-get st :emitted) 0))
             (acc (plist-get st :acc)))
        (with-current-buffer buf
          (goto-char (point-max))
          (insert chunk)
          (goto-char (point-min))
          (while (search-forward "\n" nil t)
            (let ((line (buffer-substring-no-properties (point-min) (1- (point)))))
              (delete-region (point-min) (point))
              (when-let* ((tr (string-trim-right line))
                          (parsed (lore-getter-info--parse-line tr)))
                (pcase-let ((`(,file ,node ,desc) parsed))
                  (let ((score (max 0.0 (- 1.0 (* 0.03 emitted)))))
                    (push (lore-getter-info--make-result file node desc score) acc)
                    (setq emitted (1+ emitted))
                    (when (and (functionp emit) (zerop (% emitted 10)))
                      (funcall emit (nreverse acc))
                      (setq acc nil))
                    (when (>= emitted limit)
                      (when (process-live-p proc)
                        (delete-process proc)))))))))
        (plist-put st :emitted emitted)
        (plist-put st :acc acc)
        (puthash proc st lore-getter-info--state)))))

(defun lore-getter-info--sentinel (proc _event)
  "Finalize PROC on exit."
  (let ((st (gethash proc lore-getter-info--state)))
    (when st
      (unwind-protect
          (progn
            (lore-getter-info--emit-batch proc)
            (when (functionp (plist-get st :done))
              (funcall (plist-get st :done) nil)))
        (when-let ((b (plist-get st :buf)))
          (kill-buffer b))
        (remhash proc lore-getter-info--state)))))

;;;###autoload
(cl-defun lore-getter-info-run (&key request topk emit done)
  "Run info apropos getter with REQUEST and TOPK. Stream via EMIT, finalize via DONE."
  (let* ((keywords (or (alist-get :keywords request)
                       (let ((q (alist-get :query request)))
                         (and q (list q)))))
         (pattern (lore-getter-info--pattern keywords)))
    (cond
     ((not (executable-find lore-info-program))
      (when (functionp done) (funcall done "info-not-found"))
      nil)
     ((or (null keywords) (string-empty-p pattern))
      (when (functionp done) (funcall done nil))
      nil)
     (t
      (let* ((buf (generate-new-buffer " *lore-info*"))
             (proc (make-process
                    :name "lore-info-apropos"
                    :buffer buf
                    :noquery t
                    :command (append (list lore-info-program) lore-info-extra-args (list pattern))
                    :connection-type 'pipe
                    :filter #'lore-getter-info--filter
                    :sentinel #'lore-getter-info--sentinel)))
        (puthash proc (list :buf buf
                            :emit emit
                            :done done
                            :limit (or topk most-positive-fixnum)
                            :emitted 0
                            :acc nil)
                 lore-getter-info--state)
        ;; Handle race where process may exit before state is installed.
        (unless (process-live-p proc)
          (lore-getter-info--sentinel proc "finished"))
        (list :async t
              :token (format "info-%s" (cl-incf lore-getter-info--tok))
              :cancel (lambda ()
                        (when (process-live-p proc)
                          (delete-process proc)))))))))

;; Register getter after core loads
;;;###autoload
(with-eval-after-load 'lore-core
  (when (fboundp 'lore-register-getter)
    (lore-register-getter
     'info
     :capabilities '(:domains (info) :scope (global) :kinds (doc) :match (keyword regex))
     :fn #'lore-getter-info-run
     :cost 0.8
     :batch-p t)))

(provide 'lore-getter-info)
;;; lore-getter-info.el ends here
