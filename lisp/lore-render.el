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

(defcustom lore-render-location-width 80
  "Max width of location (path/URL) shown at the end of Lore result lines."
  :type 'integer
  :group 'lore-render)

(defun lore--truncate (s n)
  (if (and s (> (length s) n))
      (concat (substring s 0 (max 0 (- n 1))) "…")
    (or s "")))

(defun lore-render--relative-path (path)
  "Return PATH relative to project root when possible, otherwise abbreviated."
  (let* ((abs (expand-file-name (or path "")))
         (root (when (fboundp 'project-current)
                 (when-let ((pr (project-current nil)))
                   (car (project-roots pr))))))
    (if (and root (string-prefix-p (file-name-as-directory (expand-file-name root))
                                   (file-name-as-directory abs)))
        (file-relative-name abs (expand-file-name root))
      (abbreviate-file-name abs))))

(defun lore-render-lines (results &optional header)
  "Return list of propertized lines for RESULTS. Optional HEADER string."
  (let ((items '()))
    (dolist (r results)
      (let* ((title (or (lore-result-title r) ""))
             (snip (lore--truncate (or (lore-result-snippet r) "") lore-render-snippet-width))
             (src (or (symbol-name (lore-result-source r)) ""))
             (score (format "%.2f" (or (lore-result-score r) 0.0)))
             (loc (cond
                   ((lore-result-path r)
                    (format " (%s)" (lore--truncate (lore-render--relative-path (lore-result-path r)) lore-render-location-width)))
                   ((lore-result-url r)
                    (format " <%s>" (lore--truncate (lore-result-url r) lore-render-location-width)))
                   (t "")))
             (line (format "%s — %s  [%s %s]%s" title snip src score loc)))
        (push (propertize line
                          'lore-result r
                          'lore-key (lore-result-key r)
                          'mouse-face 'highlight)
              items)))
    (let ((items (nreverse items)))
      (if header
          (cons header items)
        items))))

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
