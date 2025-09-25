;;; lore-core-weight-integration-test.el --- Integration tests for source weights -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-core)
(require 'lore-model)

;; Helpers

(defun lore-test--mk (title score source)
  (lore-result-create :type 'doc :title title :snippet title :score score :source source))

(defun lore-test--await (pred &optional timeout)
  (let ((t0 (float-time))
        (to (or timeout 2.0)))
    (while (and (not (funcall pred))
                (< (- (float-time) t0) to))
      (accept-process-output nil 0.02))))

;; Dummy getters (sync + async) in custom domains to avoid interference with built-ins

(cl-defun lore-test--getter-dummy-elisp-sync (&key request topk &allow-other-keys)
  (ignore request topk)
  ;; Raw scores: E1=0.6, E2=0.4
  (list (lore-test--mk "E1" 0.6 'dummy-elisp)
        (lore-test--mk "E2" 0.4 'dummy-elisp)))

(cl-defun lore-test--getter-dummy-grep-sync (&key request topk &allow-other-keys)
  (ignore request topk)
  ;; Raw scores: G1=0.5, G2=0.3
  (list (lore-test--mk "G1" 0.5 'dummy-grep)
        (lore-test--mk "G2" 0.3 'dummy-grep)))

(cl-defun lore-test--getter-dummy-grep-async (&key request topk emit done &allow-other-keys)
  (ignore request topk)
  (let ((t1 (run-at-time 0.01 nil (lambda ()
                                    (when (functionp emit)
                                      (funcall emit (list (lore-test--mk "G1" 0.5 'dummy-grep)))))))
        (t2 (run-at-time 0.03 nil (lambda ()
                                    (when (functionp emit)
                                      (funcall emit (list (lore-test--mk "G2" 0.3 'dummy-grep))))
                                    (when (functionp done)
                                      (funcall done nil))))))
    (list :async t
          :token "dummy-grep-async"
          :cancel (lambda ()
                    (when (timerp t1) (cancel-timer t1))
                    (when (timerp t2) (cancel-timer t2))))))

;; Tests

(ert-deftest lore-core/weights-pipeline-sync-integration ()
  "Weighted ranking should bias sources in the sync pipeline."
  (let* ((name-e 'dummy-elisp-sync)
         (name-g 'dummy-grep-sync)
         (caps-e '(:domains (dummy-elisp) :scope (global project) :kinds (doc)))
         (caps-g '(:domains (dummy-grep)  :scope (global project) :kinds (doc))))
    (unwind-protect
        (progn
          (lore-register-getter name-e :capabilities caps-e :fn #'lore-test--getter-dummy-elisp-sync :cost 0.5 :batch-p nil)
          (lore-register-getter name-g :capabilities caps-g :fn #'lore-test--getter-dummy-grep-sync  :cost 0.5 :batch-p nil)
          ;; Bias dummy-grep higher than dummy-elisp
          (let* ((lore-source-weights '((dummy-elisp . 1.0) (dummy-grep . 2.0)))
                 (req '((:keywords . ("x"))
                        (:targets . (dummy-elisp dummy-grep))
                        (:scope . global)
                        (:max-k . 10)))
                 (plan (lore-plan req))
                 (out (lore-run plan))
                 (titles (mapcar #'lore-result-title out)))
            ;; Expected: after weighting, G1 (0.5*2=1.0) outranks E1 (0.6*1=0.6)
            (should (equal (cl-subseq titles 0 2) '("G1" "E1")))))
      (lore-unregister-getter name-e)
      (lore-unregister-getter name-g))))

(ert-deftest lore-core/weights-pipeline-async-integration ()
  "Weighted ranking should bias sources in the async pipeline as well."
  (let* ((name-e 'dummy-elisp-sync2)
         (name-g 'dummy-grep-async)
         (caps-e '(:domains (dummy-elisp) :scope (global project) :kinds (doc)))
         (caps-g '(:domains (dummy-grep)  :scope (global project) :kinds (doc)))
         (done nil)
         (final '()))
    (unwind-protect
        (progn
          (lore-register-getter name-e :capabilities caps-e :fn #'lore-test--getter-dummy-elisp-sync :cost 0.5 :batch-p nil)
          (lore-register-getter name-g :capabilities caps-g :fn #'lore-test--getter-dummy-grep-async :cost 0.5 :batch-p t)
          (let* ((lore-source-weights '((dummy-elisp . 1.0) (dummy-grep . 2.0)))
                 (req '((:keywords . ("x"))
                        (:targets . (dummy-elisp dummy-grep))
                        (:scope . global)
                        (:max-k . 10)))
                 (plan (lore-plan req)))
            (lore-run-async plan
                            (lambda (ev payload)
                              (when (eq ev :done)
                                (setq final payload
                                      done t))))
            (lore-test--await (lambda () done) 1.0)
            (should done)
            (let ((titles (mapcar #'lore-result-title final)))
              (should (member "G1" titles))
              (should (member "E1" titles))
              (should (< (cl-position "G1" titles) (cl-position "E1" titles))))))
      (lore-unregister-getter name-e)
      (lore-unregister-getter name-g))))

(provide 'lore-core-weight-integration-test)
;;; lore-core-weight-integration-test.el ends here
