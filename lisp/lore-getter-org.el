;;; lore-getter-org.el --- Org headlines getter for Lore.el  -*- lexical-binding: t; -*-
;; Author: Lore.el
;; Keywords: convenience, outlines
;; Package-Requires: ((emacs "27.1") (org "9.1"))
;; URL: https://example.invalid/lore
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Synchronous getter that scans org files under configured roots and returns
;; matching headlines as Lore results. Focuses on simplicity and determinism.
;;
;; Contract of getter:
;;   (lore-getter-org-run &key request topk emit done) → list<lore-result>
;;
;; Respects filters:
;;   - :filters (:tag "tag") → only headlines with TAG
;;   - :keywords list<string> → matched in headline title (case-insensitive)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-element)

(require 'lore-model nil t)
(require 'lore-core  nil t)

(defgroup lore-getter-org nil
  "Org getter for Lore.el."
  :group 'lore)

(defcustom lore-org-roots nil
  "List of directories to scan for Org files.
When nil, uses `org-directory' if set, otherwise `default-directory'."
  :type '(repeat directory)
  :group 'lore-getter-org)

(defcustom lore-org-file-glob "\\.org\\'"
  "Regexp to match Org files."
  :type 'regexp
  :group 'lore-getter-org)

(defun lore-getter-org--roots ()
  "Resolve roots list."
  (or lore-org-roots
      (and (boundp 'org-directory) (list (expand-file-name org-directory)))
      (list default-directory)))

(defun lore-getter-org--collect-files ()
  "Collect Org files under roots."
  (cl-loop for root in (lore-getter-org--roots)
           when (file-directory-p root)
           nconc (directory-files-recursively root lore-org-file-glob)))

(defun lore-getter-org--headline->result (file head score)
  "Convert HEAD in FILE to lore-result with SCORE."
  (let* ((title (or (org-element-property :raw-value head) ""))
         (begin (or (org-element-property :begin head) 1))
         (level (or (org-element-property :level head) 1))
         (tags  (or (org-element-property :tags head) nil))
         (abs   (expand-file-name file)))
    (if (fboundp 'lore-result-create)
        (lore-result-create
         :type 'file
         :title title
         :snippet (format "%s:%d %s" (file-name-nondirectory abs) level title)
         :content nil
         :path abs
         :url nil
         :buffer nil
         :beg nil
         :end nil
         :score (float score)
         :source 'org
         :meta (list :pos begin :level level :tags tags))
      ;; Fallback alist
      `((type . file)
        (title . ,title)
        (snippet . ,(format "%s:%d %s" (file-name-nondirectory abs) level title))
        (content . nil)
        (path . ,abs)
        (url . nil)
        (buffer . nil)
        (beg . nil)
        (end . nil)
        (score . ,(float score))
        (source . org)
        (meta . ,(list :pos begin :level level :tags tags))))))

(defun lore-getter-org--match-title-p (title keywords)
  "Return non-nil if TITLE matches all KEYWORDS (case-insensitive)."
  (let ((case-fold-search t))
    (cl-every (lambda (kw) (string-match-p (regexp-quote kw) title)) keywords)))

(defun lore-getter-org--match-tags-p (tags wanted)
  "Return non-nil if TAGS list contains WANTED (string) or WANTED is nil."
  (or (null wanted)
      (and (listp tags) (member wanted tags))))

;;;###autoload
(cl-defun lore-getter-org-run (&key request topk emit done)
  "Scan org roots and return matching headlines as list of lore-result.
REQUEST is an alist with :keywords (list<string>) and :filters (alist)."
  (ignore emit done) ;; synchronous getter
  (let* ((keywords (or (plist-get request :keywords) '()))
         (filters  (plist-get request :filters))
         (tag      (plist-get filters :tag))
         (limit    (or topk 20))
         (results  '()))
    (dolist (file (lore-getter-org--collect-files))
      (with-temp-buffer
        (insert-file-contents file)
        (delay-mode-hooks (org-mode))
        (let ((ast (org-element-parse-buffer 'headline)))
          (org-element-map ast 'headline
            (lambda (h)
              (let* ((title (or (org-element-property :raw-value h) ""))
                     (tags  (or (org-element-property :tags h) nil)))
                (when (and (lore-getter-org--match-title-p title keywords)
                           (lore-getter-org--match-tags-p tags tag))
                  (push (lore-getter-org--headline->result
                         file h (max 0.0 (- 1.0 (* 0.02 (length results)))))
                        results))))))
        (when (>= (length results) limit)
          (cl-return))))
    (nreverse (cl-subseq results 0 (min (length results) limit)))))

;; Register getter when core is available.
;;;###autoload
(with-eval-after-load 'lore-core
  (when (fboundp 'lore-register-getter)
    (lore-register-getter
     'org
     :capabilities '(:match (keyword)
                            :scope (project global)
                            :kinds (doc file selection)
                            :domains (org))
     :fn #'lore-getter-org-run
     :cost 0.5
     :batch-p nil)))

(provide 'lore-getter-org)
;;; lore-getter-org.el ends here
