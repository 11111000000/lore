;;; lore-cache-test.el --- Tests for lore-cache -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-cache)

(ert-deftest lore-cache/put-get-and-expire ()
  (let ((lore-cache-ttl 0.1))
    (let ((key "foo") (val '(1 2 3)))
      (should (null (lore-cache-get key)))
      (lore-cache-put key val)
      (should (equal (lore-cache-get key) val))
      (sleep-for 0.15)
      (should (null (lore-cache-get key))))))

(ert-deftest lore-cache/request-fingerprint-stability ()
  (let ((req '((:query . "elisp: map ?k=5")
               (:keywords . ("map"))
               (:intent . search)
               (:filters . ((:tag . "util")))
               (:targets . (elisp))
               (:scope . global)
               (:max-k . 5))))
    (let ((f1 (lore-request-fingerprint req))
          (f2 (lore-request-fingerprint (copy-tree req))))
      (should (equal f1 f2))
      ;; Order of unrelated keys should not matter for our subset
      (let* ((req2 (append '((:extra . t)) req))
             (f3 (lore-request-fingerprint req2)))
        (should (equal f1 f3))))))

(provide 'lore-cache-test)
;;; lore-cache-test.el ends here
