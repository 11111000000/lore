;;; lore-view-test.el --- Tests for lore-view insertion -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-view)
(require 'lore-render)
(require 'lore-model)

(ert-deftest lore-view/insert-at-point ()
  (save-window-excursion
    (let* ((view (get-buffer-create "*lore*"))
           (target (get-buffer-create "*lore-target*"))
           (res (lore-result-create
                 :type 'doc
                 :title "DocT"
                 :snippet "Snippet text"
                 :content "Full content here"
                 :score 1.0
                 :source 'elisp))
           (lines (lore-render-lines (list res) "Header")))
      ;; Setup two windows: left = target, right = view
      (delete-other-windows)
      (switch-to-buffer target)
      (split-window-right)
      (other-window 1)
      (switch-to-buffer view)
      (lore-view-mode)
      (lore-render-apply-to-buffer (current-buffer) lines)
      (goto-char (point-min))
      (forward-line 1) ; move to first result line
      (lore-insert-at-point)
      ;; Verify insertion into target buffer (left window)
      (other-window -1)
      (with-current-buffer target
        (goto-char (point-min))
        (should (search-forward "Full content here" nil t))))))

(provide 'lore-view-test)
;;; lore-view-test.el ends here
