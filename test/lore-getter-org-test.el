;;; lore-getter-org-test.el --- Tests for org getter -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-getter-org)
(require 'lore-model)

(defun lore-test--with-temp-dir (fn)
  (let ((dir (make-temp-file "lore-org-" t)))
    (unwind-protect
        (funcall fn dir)
      (ignore-errors (delete-directory dir t)))))

(ert-deftest lore-getter-org/headlines-and-tags ()
  (lore-test--with-temp-dir
   (lambda (dir)
     (let* ((org-file (expand-file-name "notes.org" dir)))
       (with-temp-file org-file
         (insert "* План проекта :work:\nSome body\n** Задача A :work:\n** Идея B :idea:\n"))
       (let* ((lore-org-roots (list dir))
              (req '((:keywords . ("план"))
                     (:filters . ((:tag . "work")))
                     (:scope . global)))
              (out (lore-getter-org-run :request req :topk 10)))
         (should (listp out))
         (should (> (length out) 0))
         (let ((one (car out)))
           (should (eq (lore-result-type one) 'file))
           (should (string-suffix-p "notes.org" (lore-result-path one)))
           (should (plist-get (lore-result-meta one) :pos))))))))

(ert-deftest lore-getter-org/size-and-exclude ()
  (lore-test--with-temp-dir
   (lambda (dir)
     (let* ((big (expand-file-name "big.org" dir)))
       ;; Create big file over the default size limit
       (with-temp-file big
         (dotimes (_ 100000) (insert "x")) ; ~100KB; for safety raise limit check
         (insert "* Header\n"))
       (let* ((lore-org-roots (list dir))
              (lore-org-max-file-size 1024) ; 1KB to enforce skip
              (req '((:keywords . ("Header")))))
         (let ((out (lore-getter-org-run :request req :topk 5)))
           ;; Should skip the big file, returning nothing
           (should (listp out))
           (should (= (length out) 0))))))))
