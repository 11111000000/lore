;;; lore-model-test.el --- Tests for lore-model -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-model)

(ert-deftest lore-model/key-and-uniq ()
  (let* ((r1 (lore-result-create :type 'file :path "/tmp/a.txt" :title "A" :score 0.1))
         (r2 (lore-result-create :type 'file :path "/tmp/a.txt" :title "B" :score 0.5))
         (r3 (lore-result-create :type 'url  :url "https://e"   :title "E" :score 0.2))
         (u (lore-uniq (list r1 r2 r3))))
    (should (= (length u) 2))
    (should (cl-find-if (lambda (r) (equal (lore-result-path r) "/tmp/a.txt")) u))
    (should (cl-find-if (lambda (r) (equal (lore-result-url r) "https://e")) u))))

(ert-deftest lore-model/normalize-and-rank ()
  (let* ((r1 (lore-result-create :type 'doc :title "A" :score 0.1))
         (r2 (lore-result-create :type 'doc :title "B" :score 0.3))
         (r3 (lore-result-create :type 'doc :title "C" :score 0.2))
         (norm (lore-normalize-scores (list r1 r2 r3)))
         (ranked (lore-rank norm)))
    (should (<= (lore-result-score (car ranked)) 1.0))
    (should (equal (mapcar #'lore-result-title ranked) '("B" "C" "A")))))
