;;; lore-view.el --- Minimal UI for Lore -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides M-x lore-ask to query and render results, with simple actions.

;;; Code:

(require 'cl-lib)
(require 'lore-core)
(require 'lore-render)
(require 'lore-events)
(require 'lore-log)

(defvar lore-view-buffer-name "*lore*")

(defvar lore-view-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "q") #'quit-window)
    (define-key m (kbd "g") #'lore-refresh)
    (define-key m (kbd "RET") #'lore-open)
    (define-key m (kbd "v") #'lore-preview)
    (define-key m (kbd "n") #'next-line)
    (define-key m (kbd "p") #'previous-line)
    m)
  "Keymap for `lore-view-mode'.")

(define-derived-mode lore-view-mode special-mode "Lore"
  "View for Lore results."
  (setq buffer-read-only t)
  (buffer-disable-undo))

(defvar-local lore--last-request nil)
(defvar-local lore--last-results nil)
(defvar-local lore--last-token nil)

(defun lore--current-result ()
  (let ((r (get-text-property (line-beginning-position) 'lore-result)))
    r))

;;;###autoload
(defun lore-ask (query)
  "Ask Lore with QUERY and display results."
  (interactive (list (read-string "Lore: ")))
  (let* ((buf (get-buffer-create lore-view-buffer-name))
         (req (lore-parse-query query)))
    (with-current-buffer buf
      (lore-view-mode)
      (setq lore--last-request req))
    (pop-to-buffer buf)
    (lore-render-apply-to-buffer buf (list (format "⏳ Lore: %s" query) "" "Searching..."))
    (let* ((plan (lore-plan req))
           (token
            (lore-run-async plan
                            (lambda (event payload)
                              (when (eq event :done)
                                (with-current-buffer buf
                                  (setq lore--last-results payload
                                        lore--last-token nil)
                                  (lore-render-apply-to-buffer
                                   buf
                                   (lore-render-lines payload
                                                      (format "🧭 Lore: %s" query)))))))))
      (with-current-buffer buf (setq lore--last-token token)))))

(defun lore-refresh ()
  "Refresh last Lore request."
  (interactive)
  (if (not lore--last-request)
      (user-error "No previous Lore request")
    (let ((query (alist-get :query lore--last-request)))
      (lore-ask query))))

(defun lore-open ()
  "Open current result."
  (interactive)
  (let ((r (lore--current-result)))
    (unless r (user-error "No result at point"))
    (pcase (lore-result-type r)
      ('symbol
       (let ((name (or (plist-get (lore-result-meta r) :name)
                       (intern (lore-result-title r)))))
         (cond
          ((fboundp name) (describe-function name))
          ((boundp name) (describe-variable name))
          (t (message "Unknown symbol: %S" name)))))
      ('file
       (when-let ((p (lore-result-path r)))
         (find-file p)
         (when (and (lore-result-beg r) (integerp (lore-result-beg r)))
           (goto-char (lore-result-beg r)))))
      ('url
       (when-let ((u (lore-result-url r)))
         (browse-url u)))
      (_ (message "No opener for type: %S" (lore-result-type r))))))

(defun lore-preview ()
  "Preview current result content/snippet."
  (interactive)
  (let ((r (lore--current-result)))
    (unless r (user-error "No result at point"))
    (with-help-window "*Lore Preview*"
      (princ (or (lore-result-content r)
                 (lore-result-snippet r)
                 (lore-result-title r))))))

(provide 'lore-view)
;;; lore-view.el ends here
