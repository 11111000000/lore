;;; lore-getter-man.el --- man -k getter for Lore.el  -*- lexical-binding: t; -*-
;; Author: Lore.el
;; Keywords: unix, man, docs
;; Package-Requires: ((emacs "27.1"))
;; URL: https://example.invalid/lore
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Asynchronous getter using `man -k' (apropos) to find manpages.
;; - Streams partial results via emit; finalizes with done.
;; - Produces lore-result of type 'doc with meta (:page :section :desc :name).

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'lore-model nil t)
(require 'lore-core  nil t)

(defgroup lore-getter-man nil
  "Manpage getter for Lore.el."
  :group 'lore)

(defcustom lore-man-program "man"
  "Program used to access UNIX manual pages."
  :type 'string
  :group 'lore-getter-man)

(defcustom lore-man-extra-args '("-k")
  "Extra arguments for `man'."
  :type '(repeat string)
  :group 'lore-getter-man)

(defvar lore-getter-man--state (make-hash-table :test #'eq)
  "Internal state: process → plist(:buf BUF :emit FN :done FN :limit INT :emitted INT :acc LIST).")

(defun lore-getter-man--pattern (keywords)
  "Build pattern for man -k from KEYWORDS."
  (cond
   ((null keywords) "")
   ((= (length keywords) 1) (car keywords))
   (t (mapconcat #'identity keywords " "))))

(defun lore-getter-man--parse-line (line)
  "Parse man -k LINE. Return (NAME SECTION DESC) or nil.
Expected forms:
  name (3) - description
  name (3posix) - description"
  (when (string-match "\\`\\([^[:space:]]+\\)\\s-*(\\([^)]*\\))\\s-*[-–—]\\s-*\\(.*\\)\\'" line)
    (list (match-string 1 line)
          (match-string 2 line)
          (string-trim (match-string 3 line)))))

(defun lore-getter-man--make-result (name section desc score)
  "Create lore-result for man NAME(SECTION) with DESC and SCORE."
  (let ((page (if (and section (not (string-empty-p section)))
                  (format "%s(%s)" name section)
                name)))
    (if (fboundp 'lore-result-create)
        (lore-result-create
         :type 'doc
         :title page
         :snippet desc
         :content nil
         :path nil
         :url nil
         :buffer nil
         :beg nil :end nil
         :score (float score)
         :source 'man
         :meta (list :page page :name name :section section :desc desc))
      `((type . doc)
        (title . ,page)
        (snippet . ,desc)
        (content . nil)
        (path . nil)
        (url . nil)
        (buffer . nil)
        (beg . nil) (end . nil)
        (score . ,(float score))
        (source . man)
        (meta . ,(list :page page :name name :section section :desc desc))))))

(defun lore-getter-man--emit-batch (proc)
  "Emit accumulated results for PROC."
  (let* ((st (gethash proc lore-getter-man--state))
         (emit (plist-get st :emit))
         (acc (plist-get st :acc)))
    (when (and (functionp emit) acc)
      (funcall emit (nreverse acc))
      (plist-put st :acc nil)
      (puthash proc st lore-getter-man--state))))

(defun lore-getter-man--filter (proc chunk)
  "Process filter for PROC handling CHUNK."
  (let ((st (gethash proc lore-getter-man--state)))
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
              (when-let ((parsed (lore-getter-man--parse-line (string-trim-right line))))
                (pcase-let ((`(,name ,section ,desc) parsed))
                  (let ((score (max 0.0 (- 1.0 (* 0.03 emitted)))))
                    (push (lore-getter-man--make-result name section desc score) acc)
                    (setq emitted (1+ emitted))
                    (when (and (functionp emit) (zerop (% emitted 10)))
                      (funcall emit (nreverse acc))
                      (setq acc nil))
                    (when (>= emitted limit)
                      (when (process-live-p proc)
                        (delete-process proc)))))))))
        (plist-put st :emitted emitted)
        (plist-put st :acc acc)
        (puthash proc st lore-getter-man--state)))))

(defun lore-getter-man--sentinel (proc _event)
  "Finalize PROC on exit."
  (let ((st (gethash proc lore-getter-man--state)))
    (when st
      (unwind-protect
          (progn
            (lore-getter-man--emit-batch proc)
            (when (functionp (plist-get st :done))
              (funcall (plist-get st :done) nil)))
        (when-let ((b (plist-get st :buf)))
          (kill-buffer b))
        (remhash proc lore-getter-man--state)))))

;;;###autoload
(cl-defun lore-getter-man-run (&key request topk emit done)
  "Run man -k getter with REQUEST and TOPK. Stream via EMIT, finalize via DONE."
  (let* ((keywords (or (plist-get request :keywords)
                       (and (plist-get request :query)
                            (list (plist-get request :query)))))
         (pattern (lore-getter-man--pattern keywords)))
    (cond
     ((not (executable-find lore-man-program))
      (when (functionp done) (funcall done "man-not-found"))
      nil)
     ((or (null keywords) (string-empty-p pattern))
      (when (functionp done) (funcall done nil))
      nil)
     (t
      (let* ((buf (generate-new-buffer " *lore-man*"))
             (proc (make-process
                    :name "lore-man-k"
                    :buffer buf
                    :noquery t
                    :command (append (list lore-man-program) lore-man-extra-args (list pattern))
                    :connection-type 'pipe
                    :filter #'lore-getter-man--filter
                    :sentinel #'lore-getter-man--sentinel)))
        (puthash proc (list :buf buf
                            :emit emit
                            :done done
                            :limit (or topk most-positive-fixnum)
                            :emitted 0
                            :acc nil)
                 lore-getter-man--state)
        (list :async t
              :token (format "man-%s" (cl-incf (eval-when-compile (defvar lore-getter-man--tok 0) lore-getter-man--tok)))
              :cancel (lambda ()
                        (when (process-live-p proc)
                          (delete-process proc)))))))))

;; Register getter after core loads
;;;###autoload
(with-eval-after-load 'lore-core
  (when (fboundp 'lore-register-getter)
    (lore-register-getter
     'man
     :capabilities '(:domains (man) :scope (global) :kinds (doc) :match (keyword regex))
     :fn #'lore-getter-man-run
     :cost 0.9
     :batch-p t)))

(provide 'lore-getter-man)
;;; lore-getter-man.el ends here
