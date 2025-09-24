;;; lore-integration-context.el --- Context Navigator integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Soft integration with Context Navigator. If it's not installed, all commands
;; gracefully degrade to no-ops with a user-facing message.
;;
;; Provided API:
;; - (lore-result->cn-item res) → item-alist
;; - (lore-export-to-context RESULTS) → t/nil
;;
;; If Context Navigator exposes one of these functions, we will use it:
;; - context-navigator-add-items
;; - context-navigator-insert-items
;; Otherwise we return nil.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'lore-model)

(defun lore-context-available-p ()
  "Return non-nil if Context Navigator integration points are available."
  (or (featurep 'context-navigator)
      (featurep 'context-navigator-core)
      (fboundp 'context-navigator-add-items)
      (fboundp 'context-navigator-insert-items)))

(defun lore-result->cn-item (res)
  "Convert Lore RES (lore-result) into a Context Navigator item alist."
  (let* ((type   (lore-result-type res))
         (title  (or (lore-result-title res) ""))
         (path   (lore-result-path res))
         (url    (lore-result-url res))
         (beg    (lore-result-beg res))
         (end    (lore-result-end res))
         (meta   (lore-result-meta res))
         (src    (lore-result-source res)))
    (pcase type
      ;; Selection/file
      ('file
       (let* ((m meta)
              (ln (plist-get m :line))
              (col (plist-get m :col))
              (pos (plist-get m :pos)))
         `((:type . file)
           (:title . ,title)
           (:path . ,path)
           (:line . ,ln)
           (:col . ,col)
           (:pos . ,pos)
           (:meta . ,m))))
      ('selection
       `((:type . selection)
         (:title . ,title)
         (:path . ,path)
         (:beg . ,beg)
         (:end . ,end)
         (:meta . ,meta)))
      ;; URL
      ('url
       `((:type . url)
         (:title . ,title)
         (:url . ,url)
         (:meta . ,meta)))
      ;; Doc: route by source (info/man) to string URLs
      ('doc
       (pcase src
         ('info
          (let ((file (plist-get meta :file))
                (node (plist-get meta :node)))
            (when (and file node)
              `((:type . url)
                (:title . ,(format "(%s) %s" file node))
                (:url . ,(format "info:(%s) %s" file node))
                (:meta . ,meta)))))
         ('man
          (let ((page (or (plist-get meta :page)
                          (let ((n (plist-get meta :name))
                                (s (plist-get meta :section)))
                            (when n (if s (format "%s(%s)" n s) n))))))
            (when page
              `((:type . url)
                (:title . ,page)
                (:url . ,(format "man:%s" page))
                (:meta . ,meta)))))
         (_
          ;; Fallback: treat as a doc blob
          `((:type . doc)
            (:title . ,title)
            (:content . ,(or (lore-result-content res)
                             (lore-result-snippet res)))
            (:meta . ,meta)))))
      ;; Symbol → doc blob
      ('symbol
       `((:type . doc)
         (:title . ,title)
         (:content . ,(or (lore-result-content res)
                          (lore-result-snippet res)))
         (:meta . ,meta)))
      (_ nil))))

(defun lore--context-add-items (items)
  "Try to add ITEMS to Context Navigator. Return t on success, nil otherwise."
  (cond
   ((fboundp 'context-navigator-add-items)
    (condition-case _err
        (progn (context-navigator-add-items items) t)
      (error nil)))
   ((fboundp 'context-navigator-insert-items)
    (condition-case _err
        (progn (context-navigator-insert-items items) t)
      (error nil)))
   (t nil)))

;;;###autoload
(defun lore-export-to-context (results)
  "Export list of RESULTS (lore-result) to Context Navigator.
When integration is not available, return nil and show a message."
  (interactive (list nil))
  (if (not (lore-context-available-p))
      (progn
        (when (called-interactively-p 'any)
          (message "Context Navigator not available; install and load it to export"))
        nil)
    (let* ((items (cl-remove nil (mapcar #'lore-result->cn-item results))))
      (if (null items)
          (progn
            (when (called-interactively-p 'any)
              (message "No convertible Lore results to export"))
            nil)
        (if (lore--context-add-items items)
            (progn
              (when (called-interactively-p 'any)
                (message "Exported %d item(s) to Context Navigator" (length items)))
              t)
          (when (called-interactively-p 'any)
            (message "Failed to add items to Context Navigator"))
          nil)))))

(provide 'lore-integration-context)
;;; lore-integration-context.el ends here
