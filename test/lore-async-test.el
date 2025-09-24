;;; lore-async-test.el --- Async pipeline smoke tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-core)
(require 'lore-model)

(defvar lore-test--async-events nil)

(defun lore-test--mk-result (n)
  (lore-result-create :type 'doc :title (format "R%d" n) :snippet "S" :score (/ n 10.0) :source 'async-dummy))

(cl-defun lore-test--async-getter (&key request topk emit done &allow-other-keys)
  (ignore request topk)
  ;; Emit 3 partial batches via timers, then done.
  (let ((count 0)
        (timers '()))
    (dotimes (i 3)
      (push (run-at-time (* 0.01 (1+ i)) nil
                         (lambda ()
                           (setq count (1+ count))
                           (when (functionp emit)
                             (funcall emit (list (lore-test--mk-result count))))))
            timers))
    (run-at-time 0.05 nil
                 (lambda ()
                   (when (functionp done) (funcall done nil))))
    (list :async t
          :token "async-dummy"
          :cancel (lambda ()
                    (mapc (lambda (t) (when (timerp t) (cancel-timer t))) timers)))))

(ert-deftest lore-async/partial-and-done ()
  (let ((name 'async-dummy)
        (caps '(:domains (elisp) :scope (global project) :kinds (doc))))
    (unwind-protect
        (progn
          (lore-register-getter name :capabilities caps :fn #'lore-test--async-getter :cost 0.2 :batch-p t)
          (setq lore-test--async-events nil)
          (let* ((req (lore-parse-query "elisp: async"))
                 (plan (lore-plan req)))
            (lore-run-async plan
                            (lambda (ev _payload)
                              (push ev lore-test--async-events)))
            ;; Wait a bit for timers to fire
            (sit-for 0.2)
            ;; We expect at least one :partial and one :done
            (should (member :partial lore-test--async-events))
            (should (member :done lore-test--async-events))))
      (lore-unregister-getter name))))

;;; lore-async-test.el ends here
