;;; lore-events-test.el --- Tests for lore-events -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-events)

(ert-deftest lore-events/pub-sub ()
  (let ((topic :test/hello)
        (acc '()))
    (cl-labels ((handler (&rest args) (setq acc (append acc (list args)))))
      (unwind-protect
          (progn
            (lore-events-subscribe topic #'handler)
            (lore-events-publish topic 1 2 3)
            (should (equal acc '((1 2 3)))))
        (lore-events-unsubscribe topic #'handler)))))

(ert-deftest lore-events/debounce ()
  (let ((topic :test/debounce)
        (key '(k))
        (count 0))
    (lore-events-debounce topic key 0.05 (lambda () (cl-incf count)))
    (lore-events-debounce topic key 0.05 (lambda () (cl-incf count)))
    (lore-events-debounce topic key 0.05 (lambda () (cl-incf count)))
    (sleep-for 0.08)
    ;; Only last scheduled should run once
    (should (= count 1))))

(provide 'lore-events-test)
;;; lore-events-test.el ends here
