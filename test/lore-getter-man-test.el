;;; lore-getter-man-test.el --- Tests for man getter -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-getter-man)
(require 'lore-model)

(defun lore-test--await (pred &optional timeout)
  (let ((t0 (float-time))
        (to (or timeout 5.0)))
    (while (and (not (funcall pred))
                (< (- (float-time) t0) to))
      (accept-process-output nil 0.05))))

(ert-deftest lore-getter-man/async-basic ()
  (skip-unless (executable-find lore-man-program))
  (let* ((req '((:keywords . ("printf"))
                (:scope . global)))
         (emitted '())
         (done-flag nil)
         (res '()))
    (let ((ret (lore-getter-man-run
                :request req :topk 20
                :emit (lambda (batch) (setq emitted (append emitted batch)))
                :done (lambda (&optional _err)
                        (setq done-flag t
                              res (append res emitted))))))

      (should (and (listp ret) (plist-get ret :async)))
      (lore-test--await (lambda () done-flag) 6.0)
      (should done-flag)
      ;; If we got items, ensure they look like man docs
      (when res
        (dolist (r res)
          (should (eq (lore-result-type r) 'doc))
          (should (eq (lore-result-source r) 'man))
          (let ((m (lore-result-meta r)))
            (should (or (plist-get m :page)
                        (and (plist-get m :name)
                             (plist-get m :section))))))))))

(ert-deftest lore-getter-man/generic-keyword-man ()
  "Ensure a generic keyword like \"man\" completes and tends to return something."
  (skip-unless (executable-find lore-man-program))
  (let* ((req '((:keywords . ("man"))
                (:targets . (man))
                (:scope . project)))
         (emitted '())
         (done-flag nil)
         (res '()))
    (let ((ret (lore-getter-man-run
                :request req :topk 10
                :emit (lambda (batch) (setq emitted (append emitted batch)))
                :done (lambda (&optional _err) (setq done-flag t)))))
      (should (and (listp ret) (plist-get ret :async)))
      (lore-test--await (lambda () done-flag) 6.0)
      (should done-flag)
      (setq res emitted)
      ;; Don't assert non-empty (platform-dependent), but ensure types if present
      (when res
        (dolist (r res)
          (should (eq (lore-result-type r) 'doc))
          (should (eq (lore-result-source r) 'man)))))))
