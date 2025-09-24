;;; lore-render.el --- Rendering helpers for Lore -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure rendering of results to propertized lines, and buffer apply.

;;; Code:

(require 'lore-model)

(defgroup lore-render nil
  "Rendering options for Lore."
  :group 'lore)

(defcustom lore-render-snippet-width 100
  "Max width of snippet column in Lore results."
  :type 'integer
  :group 'lore-render)

(defun lore--truncate (s n)
  (if (and s (> (length s) n))
      (concat (substring s 0 (max 0 (- n 1))) "…")
    (or s "")))

(defun lore-render-lines (results &optional header)
  "Return list of propertized lines for RESULTS. Optional HEADER string."
  (let ((lines (when header (list header ""))))
    (dolist (r results (or lines '()))
      (let* ((title (or (lore-result-title r) ""))
             (snip (lore--truncate (or (lore-result-snippet r) "") lore-render-snippet-width))
             (src (or (symbol-name (lore-result-source r)) ""))
             (score (format "%.2f" (or (lore-result-score r) 0.0)))
             (line (format "%s — %s  [%s %s]" title snip src score)))
        (push (propertize line
                          'lore-result r
                          'lore-key (lore-result-key r)
                          'mouse-face 'highlight)
              lines)))
    (nreverse lines)))

(defun lore-render-apply-to-buffer (buffer lines)
  "Replace BUFFER content with LINES."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (pt (point))
          (ws (window-start)))
      (erase-buffer)
      (dolist (l lines) (insert l "\n"))
      (goto-char (min pt (point-max)))
      (set-window-start (selected-window) ws))))

(provide 'lore-render)
;;; lore-render.el ends here
