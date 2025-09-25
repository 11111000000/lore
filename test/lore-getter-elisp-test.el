;;; lore-getter-elisp-test.el --- Tests for elisp getter -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-getter-elisp)
(require 'lore-model)

(ert-deftest lore-getter-elisp/basic-results ()
  (let* ((req '((:keywords . ("cons"))))
         (out (lore-getter-elisp :request req :topk 10)))
    (should (listp out))
    (should (> (length out) 0))
    (dolist (r out)
      (should (eq (lore-result-type r) 'symbol))
      (should (memq (lore-result-source r) '(elisp))))))

(ert-deftest lore-getter-elisp/topk-limit ()
  (let* ((req '((:keywords . ("map"))))
         (k 5)
         (out (lore-getter-elisp :request req :topk k)))
    (should (<= (length out) k))
    (dolist (r out)
      (should (eq (lore-result-type r) 'symbol)))))
