;;; lore-getter-grep-test.el --- Tests for ripgrep getter -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-getter-grep)
(require 'lore-model)

(defun lore-test--with-temp-dir (fn)
  (let ((dir (make-temp-file "lore-rg-" t)))
    (unwind-protect
        (funcall fn dir)
      (ignore-errors (delete-directory dir t)))))

(defun lore-test--write-file (path content)
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert content)))

(defun lore-test--await (pred &optional timeout)
  (let ((t0 (float-time))
        (to (or timeout 5.0)))
    (while (and (not (funcall pred))
                (< (- (float-time) t0) to))
      (accept-process-output nil 0.05))))

(ert-deftest lore-getter-grep/async-basic ()
  (skip-unless (executable-find lore-grep-program))
  (lore-test--with-temp-dir
   (lambda (dir)
     (let* ((file-a (expand-file-name "a.txt" dir))
            (file-b (expand-file-name "sub/b.el" dir)))
       (lore-test--write-file file-a "hello world\nline 2\n")
       (lore-test--write-file file-b "(defun demo () (message \"hello\"))\n")
       (let* ((req `((:keywords . ("hello"))
                     (:scope . project)
                     (:project-root . ,dir)))
              (emitted '())
              (done-flag nil)
              (res '()))
         (let ((ret (lore-getter-grep-run
                     :request req :topk 10
                     :emit (lambda (batch) (setq emitted (append emitted batch)))
                     :done (lambda (&optional _err)
                             (setq done-flag t
                                   res (append res emitted))))))

           (should (and (listp ret) (plist-get ret :async)))
           (lore-test--await (lambda () done-flag) 5.0)
           (should done-flag)
           ;; At least one match in a.txt
           (should (cl-some (lambda (r)
                              (and (eq (lore-result-type r) 'file)
                                   (string-suffix-p "a.txt" (lore-result-path r))))
                            res))
           ;; Meta has line/col
           (dolist (r res)
             (when (eq (lore-result-type r) 'file)
               (let ((m (lore-result-meta r)))
                 (should (plist-get m :line))
                 (should (plist-get m :col)))))))))))

(ert-deftest lore-getter-grep/global-scope-fallback ()
  (skip-unless (executable-find lore-grep-program))
  (lore-test--with-temp-dir
   (lambda (dir)
     (let* ((file-a (expand-file-name "readme.txt" dir)))
       (lore-test--write-file file-a "alpha beta gamma\n")
       (let* ((default-directory dir)
              (req '((:keywords . ("alpha"))
                     (:scope . global))) ; allow outside project
              (done-flag nil)
              (res '()))
         (let ((ret (lore-getter-grep-run
                     :request req :topk 5
                     :emit (lambda (batch) (setq res (append res batch)))
                     :done (lambda (&optional _err) (setq done-flag t)))))
           (should (and (listp ret) (plist-get ret :async)))
           (lore-test--await (lambda () done-flag) 5.0)
           (should done-flag)
           (should (cl-some (lambda (r)
                              (and (eq (lore-result-type r) 'file)
                                   (string-suffix-p "readme.txt" (lore-result-path r))))
                            res))))))))
