;;; lore-core-cache-test.el --- Tests for lore-core cache path -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-core)
(require 'lore-cache)
(require 'lore-model)
(require 'lore-events)

(defun lore-test--mk-doc (title score src)
  (lore-result-create :type 'doc :title title :score score :source src))

(ert-deftest lore-core/cache-hit-sync-and-async ()
  (let* ((req '((:query . "elisp: map")
                (:keywords . ("map"))
                (:targets . (elisp))
                (:scope . project)
                (:max-k . 10)))
         (key (lore-request-fingerprint req))
         (val (list (lore-test--mk-doc "T" 0.5 'elisp))))
    (let ((lore-cache-enabled t)
          (lore-cache-ttl 5.0))
      ;; Pre-populate cache
      (lore-cache-put key val)
      ;; Sync path returns cached value
      (let* ((plan (list :request req :getters '()))
             (out (lore-run plan)))
        (should (equal (mapcar #'lore-result-title out) '("T"))))
      ;; Async path should publish events and return nil token
      (let ((started nil) (done nil) (payload nil))
        (lore-events-subscribe :lore-query-start (lambda (&rest _args) (setq started t)))
        (lore-events-subscribe :lore-done (lambda (_tok res) (setq done t payload res)))
        (unwind-protect
            (let ((tok (lore-run-async (list :request req :getters '())
                                       (lambda (ev res)
                                         (when (eq ev :done) (setq payload res done t))))))
              (should (null tok))
              (should started)
              (should done)
              (should (equal (mapcar #'lore-result-title payload) '("T"))))
          (lore-events-unsubscribe :lore-query-start (lambda (&rest _args) (setq started t)))
          (lore-events-unsubscribe :lore-done (lambda (_tok res) (setq done t payload res))))))))

(provide 'lore-core-cache-test)
;;; lore-core-cache-test.el ends here
