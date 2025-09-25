;;; lore-render-path-test.el --- Tests for relative path rendering -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-render)
(require 'lore-model)

(ert-deftest lore-render/relative-path-fallback-and-project ()
  (let* ((tmp (make-temp-file "lore-rend-" t))
         (file (expand-file-name "sub/dir/file.txt" tmp)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert "x"))
    ;; Fallback renders abbreviated absolute path
    (let* ((r (lore-result-create :type 'file :title "f" :path file :source 'grep))
           (ls (lore-render-lines (list r) nil))
           (line (car ls)))
      (should (string-match (regexp-quote (file-name-nondirectory file)) line))
      (should (string-match (regexp-quote (abbreviate-file-name (file-name-directory tmp))) line)))
    ;; With project stubs, ensure path is relative
    (cl-letf* (((symbol-function 'project-current)
                (lambda (&optional _prompt) 'dummy))
               ((symbol-function 'project-roots)
                (lambda (_pr) (list tmp))))
      (let* ((r (lore-result-create :type 'file :title "f" :path file :source 'grep))
             (ls (lore-render-lines (list r) nil))
             (line (car ls)))
        (should (string-match (regexp-quote "sub/dir/file.txt") line))))))

(provide 'lore-render-path-test)
;;; lore-render-path-test.el ends here
