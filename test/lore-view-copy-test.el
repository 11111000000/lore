;;; lore-view-copy-test.el --- Tests for lore-view copy command -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-view)
(require 'lore-render)
(require 'lore-model)

(defun lore-test--with-view (results fn)
  (let* ((buf (get-buffer-create "*lore*"))
         (lines (lore-render-lines results "Header")))
    (with-current-buffer buf
      (lore-view-mode)
      (lore-render-apply-to-buffer buf lines)
      (goto-char (point-min))
      (forward-line 1)
      (funcall fn buf))))

(ert-deftest lore-view/copy-file-and-url-and-doc ()
  ;; file
  (let ((r (lore-result-create :type 'file :title "t" :path "/tmp/foo/bar.txt" :source 'grep)))
    (lore-test--with-view (list r)
                          (lambda (_buf)
                            (lore-copy-result)
                            (should (string-suffix-p "bar.txt" (current-kill 0 t))))))
  ;; url
  (let ((r (lore-result-create :type 'url :title "U" :url "https://example.org" :source 'web)))
    (lore-test--with-view (list r)
                          (lambda (_buf)
                            (lore-copy-result)
                            (should (string-prefix-p "https://" (current-kill 0 t))))))
  ;; info doc
  (let* ((meta (list :file "emacs" :node "Buffers"))
         (r (lore-result-create :type 'doc :title "(emacs) Buffers" :source 'info :meta meta)))
    (lore-test--with-view (list r)
                          (lambda (_buf)
                            (lore-copy-result)
                            (should (string-match-p "^(emacs) Buffers" (current-kill 0 t)))))))

(provide 'lore-view-copy-test)
;;; lore-view-copy-test.el ends here
