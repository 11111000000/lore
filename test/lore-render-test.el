;;; lore-render-test.el --- Tests for lore-render -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-render)
(require 'lore-model)

(ert-deftest lore-render/lines-basic ()
  (let* ((r (lore-result-create :type 'doc :title "T" :snippet "S" :source 'elisp))
         (ls (lore-render-lines (list r) "Header")))
    (should (string-match-p "^Header" (car ls)))
    (should (< 1 (length ls)))
    (should (string-match-p "T — S" (cadr ls)))))
