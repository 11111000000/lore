;;; lore-core-weight-test.el --- Tests for source weights in ranking -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-core)
(require 'lore-model)

(ert-deftest lore-core/weights-affect-ranking ()
  "Heavier source weight should lift its results before normalization."
  (let* ((lore-source-weights '((elisp . 1.0) (grep . 2.0)))
         (r-el (lore-result-create :type 'doc :title "E" :score 0.6 :source 'elisp))
         (r-gr (lore-result-create :type 'doc :title "G" :score 0.5 :source 'grep))
         ;; Apply weighting then normalize+rank like pipeline
         (weighted (lore--apply-source-weights (list r-el r-gr)))
         (ranked (lore-rank (lore-normalize-scores weighted))))
    ;; After weighting, grep(0.5*2=1.0) should outrank elisp(0.6*1=0.6)
    (should (equal (mapcar #'lore-result-title ranked) '("G" "E")))))

(provide 'lore-core-weight-test)
;;; lore-core-weight-test.el ends here
