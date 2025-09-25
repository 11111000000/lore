;;; lore-core-eligible-test.el --- Tests for eligible getter selection -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-core)

(defun lore-test--mk-getter (name caps)
  (list :name name :capabilities caps :fn (lambda (&rest _) nil) :cost 1.0 :batch-p nil))

(ert-deftest lore-core/eligible-by-targets-and-scope ()
  (let* ((g-el (lore-test--mk-getter 'el '(:domains (elisp) :scope (global project))))
         (g-web (lore-test--mk-getter 'web '(:domains (web) :scope (global))))
         (req (lore-parse-query "elisp: map ?scope=project")))
    (let* ((plan (let ((all (list g-el g-web)))
                   (cl-letf (((symbol-function 'lore-getters) (lambda () all)))
                     (lore-plan req)))))
      (let ((names (mapcar (lambda (c) (plist-get c :name)) (plist-get plan :getters))))
        (should (member 'el names))
        (should (not (member 'web names)))))))

(provide 'lore-core-eligible-test)
;;; lore-core-eligible-test.el ends here
