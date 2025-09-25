;;; lore-getter-info-test.el --- Tests for info getter -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-getter-info)
(require 'lore-model)

(defun lore-test--await (pred &optional timeout)
  (let ((t0 (float-time))
        (to (or timeout 5.0)))
    (while (and (not (funcall pred))
                (< (- (float-time) t0) to))
      (accept-process-output nil 0.05))))

(ert-deftest lore-getter-info/async-basic ()
  (skip-unless (executable-find lore-info-program))
  (let* ((req '((:keywords . ("emacs")) ; broad term, usually present
                (:scope . global)))
         (emitted '())
         (done-flag nil)
         (res '()))
    (let ((ret (lore-getter-info-run
                :request req :topk 20
                :emit (lambda (batch) (setq emitted (append emitted batch)))
                :done (lambda (&optional _err)
                        (setq done-flag t
                              res (append res emitted))))))

      (should (and (listp ret) (plist-get ret :async)))
      (lore-test--await (lambda () done-flag) 6.0)
      (should done-flag)
      ;; If we got items, ensure they look like info docs
      (when res
        (dolist (r res)
          (should (eq (lore-result-type r) 'doc))
          (should (eq (lore-result-source r) 'info))
          (let ((m (lore-result-meta r)))
            (should (plist-get m :file))
            (should (plist-get m :node))))))))
