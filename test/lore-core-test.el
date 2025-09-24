;;; lore-core-test.el --- Tests for lore-core plan/registry -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-core)
(require 'lore-model)

(defun lore-test--dummy-getter (&key request topk &allow-other-keys)
  (ignore request topk)
  (list (lore-result-create :type 'doc :title "X" :snippet "Y" :score 1.0 :source 'dummy)))

(ert-deftest lore-core/registry-and-plan ()
  (let ((name 'dummy)
        (caps '(:domains (elisp) :scope (global) :kinds (doc))))
    (unwind-protect
        (progn
          (lore-register-getter name :capabilities caps :fn #'lore-test--dummy-getter :cost 0.2 :batch-p nil)
          (let* ((req (lore-parse-query "elisp: map"))
                 (plan (lore-plan req))
                 (calls (plist-get plan :getters)))
            (should (> (length calls) 0))
            (should (equal (plist-get (car calls) :name) name))))
      (lore-unregister-getter name))))

(ert-deftest lore-core/run-sync ()
  (let ((name 'dummy2)
        (caps '(:domains (elisp) :scope (global) :kinds (doc))))
    (unwind-protect
        (progn
          (lore-register-getter name :capabilities caps :fn #'lore-test--dummy-getter :cost 0.2 :batch-p nil)
          (let* ((req (lore-parse-query "elisp: thing"))
                 (plan (lore-plan req))
                 (out (lore-run plan)))
            (should (listp out))
            (should (> (length out) 0))))
      (lore-unregister-getter name))))
