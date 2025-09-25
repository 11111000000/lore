;;; lore-cancel-test.el --- Tests for lore-cancel behavior -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-core)
(require 'lore-model)
(require 'lore-events)

(defun lore-test--await (pred &optional timeout)
  (let ((t0 (float-time))
        (to (or timeout 1.0)))
    (while (and (not (funcall pred))
                (< (- (float-time) t0) to))
      (accept-process-output nil 0.02))))

;; Mock async getter that never calls done unless canceled.
(cl-defun lore-test--never-done-getter (&key request topk emit done &allow-other-keys)
  (ignore request topk emit)
  ;; Set up a timer that would emit later; cancel should stop it.
  (let* ((emitted nil)
         (t1 (run-at-time 0.5 nil (lambda ()
                                    (setq emitted t)
                                    (when (functionp emit)
                                      (funcall emit (list (lore-result-create
                                                           :type 'doc :title "ND" :score 0.1 :source 'nd))))))))
    (list :async t
          :token "never-done"
          :cancel (lambda ()
                    (when (timerp t1) (cancel-timer t1))
                    (when (functionp done) (funcall done nil))))))

(ert-deftest lore-core/cancel-publishes-event-and-no-done ()
  (let* ((name 'never-done)
         (caps '(:domains (elisp) :scope (global project) :kinds (doc)))
         (got-cancel nil)
         (got-done nil))
    (unwind-protect
        (progn
          (lore-register-getter name :capabilities caps :fn #'lore-test--never-done-getter :cost 0.1 :batch-p t)
          (lore-events-subscribe :lore-cancel (lambda (&rest _a) (setq got-cancel t)))
          (lore-events-subscribe :lore-done   (lambda (&rest _a) (setq got-done t)))
          (let* ((req (lore-parse-query "elisp: x"))
                 (plan (lore-plan req))
                 (tok (lore-run-async plan (lambda (ev _payload)
                                             (when (eq ev :done) (setq got-done t))))))
            (should (stringp tok))
            ;; Immediately cancel and ensure cancel event is published
            (should (lore-cancel tok))
            (lore-test--await (lambda () got-cancel) 0.3)
            (should got-cancel)
            ;; Should not observe :done after cancel (give a small grace period)
            (lore-test--await (lambda () got-done) 0.1)
            (should (not got-done))))
      ;; Cleanup
      (lore-unregister-getter name)
      (lore-events-unsubscribe :lore-cancel (lambda (&rest _a) (setq got-cancel t)))
      (lore-events-unsubscribe :lore-done   (lambda (&rest _a) (setq got-done t))))))

(provide 'lore-cancel-test)
;;; lore-cancel-test.el ends here
