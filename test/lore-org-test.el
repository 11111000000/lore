;;; lore-org-test.el --- Tests for lore-getter-org -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-getter-org)

(defun lore-test--with-temp-dir (fn)
  (let* ((dir (make-temp-file "lore-org" t)))
    (unwind-protect
        (funcall fn dir)
      (ignore-errors (delete-directory dir t)))))

(defun lore-test--write-file (path content)
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert content))
  path)

(ert-deftest lore-org/tag-filter-and-keywords ()
  (lore-test--with-temp-dir
   (lambda (dir)
     (let* ((org-file (expand-file-name "notes.org" dir))
            (_ (lore-test--write-file
                org-file
                "* Heading :foo:\nBody here\n* Other\n")))
       (let* ((lore-org-roots (list dir))
              (req `((:keywords . ("Heading"))
                     (:filters . ((:tag . "foo")))
                     (:scope . global)))
              (out (lore-getter-org-run :request req :topk 10)))
         (should (listp out))
         (should (> (length out) 0))
         (should (string-match-p "Heading" (lore-result-title (car out)))))))))

(ert-deftest lore-org/size-limit-skip ()
  (lore-test--with-temp-dir
   (lambda (dir)
     (let* ((big (expand-file-name "big.org" dir))
            ;; Create > 10KB file
            (_ (lore-test--write-file big (make-string (+ 12000 1) ?x)))
            (lore-org-roots (list dir))
            (lore-org-max-file-size 10240) ; 10KB
            (req '((:keywords . ("whatever")) (:filters . ()) (:scope . global)))
            (out (lore-getter-org-run :request req :topk 5)))
       ;; No results (file skipped), but also no error
       (should (listp out))
       (should (zerop (length out)))))))

(provide 'lore-org-test)
;;; lore-org-test.el ends here
