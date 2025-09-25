;;; lore-view.el --- Minimal UI for Lore -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides M-x lore-ask to query and render results, with simple actions.
;; Adds async spinner and cached indicator.

;;; Code:

(eval-and-compile
  (add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name))))

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
    (define-key m (kbd "r") #'lore-refine)
    (define-key m (kbd "t") #'lore-transient)
    (define-key m (kbd "a") #'lore-export-to-context)
    (define-key m (kbd "c") #'lore-copy-result)
    (define-key m (kbd "i") #'lore-insert-at-point)
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
(defvar-local lore--last-query nil)

;; Spinner state
(defvar lore--spinner-frames '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"))
(defvar-local lore--spinner-index 0)
(defvar-local lore--spinner-timer nil)

(defun lore--spinner-stop ()
  "Stop spinner timer if running."
  (when (timerp lore--spinner-timer)
    (cancel-timer lore--spinner-timer))
  (setq lore--spinner-timer nil
        lore--spinner-index 0))

(defun lore--spinner-start ()
  "Start spinner that re-renders header with frames."
  (lore--spinner-stop)
  (let ((buf (current-buffer)))
    (setq lore--spinner-index 0
          lore--spinner-timer
          (run-at-time 0 0.15
                       (lambda ()
                         (when (buffer-live-p buf)
                           (with-current-buffer buf
                             (when lore--last-token
                               (let* ((frame (nth (% lore--spinner-index (length lore--spinner-frames))
                                                  lore--spinner-frames))
                                      (header (format "%s Lore: %s" frame (or lore--last-query "")))
                                      (lines (lore-render-lines (or lore--last-results '()) header)))
                                 (cl-incf lore--spinner-index)
                                 (lore-render-apply-to-buffer buf lines))))))))))

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
      ;; Cancel previous running token if any
      (when lore--last-token
        (ignore-errors (lore-cancel lore--last-token)))
      (setq lore--last-request req
            lore--last-results nil
            lore--last-token nil
            lore--last-query query))
    (pop-to-buffer buf)
    ;; Initial header
    (lore-render-apply-to-buffer buf (list (format "⏳ Lore: %s" query) "" "Searching..."))
    (let* ((plan (lore-plan req))
           (token
            (lore-run-async plan
                            (lambda (event payload)
                              (with-current-buffer buf
                                (let ((cached-p (null token)))
                                  (pcase event
                                    (:partial
                                     ;; Update results only; spinner will re-render frequently.
                                     (setq lore--last-results payload))
                                    (:done
                                     (setq lore--last-results payload
                                           lore--last-token nil)
                                     (lore--spinner-stop)
                                     (let* ((n (length payload))
                                            (hdr (format "🧭 Lore: %s — %d result%s%s"
                                                         query n (if (= n 1) "" "s")
                                                         (if cached-p " (cached)" ""))))
                                       (lore-render-apply-to-buffer
                                        buf
                                        (append
                                         (lore-render-lines payload hdr)
                                         (list ""
                                               "q quit  g refresh  r refine  t transient  a export  c copy  RET open  v preview  n/p move"))))))))))))
      (with-current-buffer buf
        (setq lore--last-token token))
      ;; Start spinner only if token is non-nil (not cached)
      (when token
        (with-current-buffer buf
          (lore--spinner-start))))))

(defun lore-refresh ()
  "Refresh last Lore request."
  (interactive)
  (if (not lore--last-request)
      (user-error "No previous Lore request")
    (let ((query (alist-get :query lore--last-request)))
      (lore-ask query))))

(defun lore-refine ()
  "Refine last Lore request by editing the query string."
  (interactive)
  (if (not lore--last-request)
      (user-error "No previous Lore request")
    (let* ((old (alist-get :query lore--last-request))
           (q (read-string "Refine Lore: " old)))
      (lore-ask q))))

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
         (let* ((m (lore-result-meta r))
                (ln (plist-get m :line))
                (col (plist-get m :col))
                (pos (plist-get m :pos)))
           (cond
            ((and (integerp ln) (> ln 0))
             (goto-char (point-min))
             (forward-line (1- ln))
             (when (and (integerp col) (> col 0))
               (move-to-column (1- col))))
            ((and (integerp pos) (> pos 0))
             (goto-char pos))
            ((and (lore-result-beg r) (integerp (lore-result-beg r)))
             (goto-char (lore-result-beg r)))))))
      ('url
       (when-let ((u (lore-result-url r)))
         (browse-url u)))
      ('doc
       (pcase (lore-result-source r)
         ('info
          (let* ((m (lore-result-meta r))
                 (file (plist-get m :file))
                 (node (plist-get m :node)))
            (if (and file node)
                (info (format "(%s) %s" file node))
              (message "Info node not specified"))))
         ('man
          (let* ((m (lore-result-meta r))
                 (page (or (plist-get m :page)
                           (let ((name (plist-get m :name))
                                 (sec (plist-get m :section)))
                             (if (and name sec) (format "%s(%s)" name sec) name)))))
            (if page
                (man page)
              (message "Man page not specified"))))
         (_
          (with-help-window "*Lore Doc*"
            (princ (or (lore-result-content r)
                       (lore-result-snippet r)
                       (lore-result-title r)))))))
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

(defun lore-view--collect-selected-results ()
  "Collect Lore results from current line or active region in view buffer."
  (let ((results '()))
    (save-excursion
      (if (use-region-p)
          (let ((beg (region-beginning))
                (end (region-end)))
            (goto-char beg)
            (beginning-of-line)
            (while (< (point) end)
              (when-let ((r (get-text-property (point) 'lore-result)))
                (push r results))
              (forward-line 1)))
        (beginning-of-line)
        (when-let ((r (get-text-property (point) 'lore-result)))
          (push r results))))
    (nreverse results)))

;;;###autoload
(defun lore-export-to-context ()
  "Export selected results to Context Navigator (if available).
If a region is active, export all results in the region; otherwise, current line."
  (interactive)
  (let ((results (lore-view--collect-selected-results)))
    (if (null results)
        (user-error "No Lore results selected")
      (if (require 'lore-integration-context nil t)
          (if (lore-export-to-context results)
              (message "Exported %d item(s) to Context Navigator" (length results))
            (message "Context Navigator rejected items"))
        (message "Context Navigator not available; install it to export")))))

(defun lore-copy-result ()
  "Copy current result's useful reference to `kill-ring'."
  (interactive)
  (let ((r (lore--current-result)))
    (unless r (user-error "No result at point"))
    (let* ((kind (lore-result-type r))
           (meta (lore-result-meta r))
           (text
            (pcase kind
              ('url (or (lore-result-url r) (lore-result-title r)))
              ('file
               (or (lore-result-path r)
                   (lore-result-title r)))
              ('symbol
               (format "%s" (or (plist-get meta :name)
                                (lore-result-title r))))
              ('doc
               (pcase (lore-result-source r)
                 ('info (let ((file (plist-get meta :file))
                              (node (plist-get meta :node)))
                          (if (and file node) (format "(%s) %s" file node)
                            (lore-result-title r))))
                 ('man (or (plist-get meta :page) (lore-result-title r)))
                 (_ (lore-result-title r))))
              (_ (or (lore-result-title r)
                     (lore-result-snippet r))))))
      (kill-new text)
      (message "Copied: %s" text))))

(defun lore-view--pick-target-window ()
  "Pick a window to insert into (not the Lore view). Return window or nil."
  (cl-loop for w in (window-list)
           for b = (window-buffer w)
           unless (string= (buffer-name b) lore-view-buffer-name)
           return w))

;;;###autoload
(defun lore-insert-at-point ()
  "Insert selected result's content/snippet/title at point in another window/buffer.
If a non-Lore window exists, insert there; otherwise error."
  (interactive)
  (let ((r (lore--current-result)))
    (unless r (user-error "No result at point"))
    (let ((text (or (lore-result-content r)
                    (lore-result-snippet r)
                    (lore-result-title r))))
      (unless (and text (stringp text))
        (user-error "No text content available for insertion"))
      (let ((win (lore-view--pick-target-window)))
        (unless (window-live-p win)
          (user-error "No target window to insert into; split window and try again"))
        (with-selected-window win
          (insert text)
          (message "Inserted %d chars" (length text)))))))

(provide 'lore-view)
;;; lore-view.el ends here
