;;; lore-integration-context-test.el --- Tests for context integration -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-integration-context)
(require 'lore-model)

(ert-deftest lore-context/convert-file-selection-url-doc ()
  ;; file with line/col
  (let* ((r1 (lore-result-create :type 'file :title "A" :path "/tmp/a.txt"
                                 :meta (list :line 10 :col 3)))
         (i1 (lore-result->cn-item r1)))
    (should (equal (alist-get :type i1) 'file))
    (should (equal (alist-get :path i1) "/tmp/a.txt"))
    (should (= (alist-get :line i1) 10)))
  ;; url
  (let* ((r2 (lore-result-create :type 'url :title "U" :url "https://e"))
         (i2 (lore-result->cn-item r2)))
    (should (equal (alist-get :type i2) 'url))
    (should (string-prefix-p "https://" (alist-get :url i2))))
  ;; info doc
  (let* ((meta (list :file "emacs" :node "Buffers"))
         (r3 (lore-result-create :type 'doc :title "(emacs) Buffers" :source 'info :meta meta))
         (i3 (lore-result->cn-item r3)))
    (should (equal (alist-get :type i3) 'url))
    (should (string-prefix-p "info:" (alist-get :url i3))))
  ;; man doc
  (let* ((meta (list :name "printf" :section "3"))
         (r4 (lore-result-create :type 'doc :title "printf(3)" :source 'man :meta meta))
         (i4 (lore-result->cn-item r4)))
    (should (equal (alist-get :type i4) 'url))
    (should (string-prefix-p "man:" (alist-get :url i4)))))

(provide 'lore-integration-context-test)
;;; lore-integration-context-test.el ends here
