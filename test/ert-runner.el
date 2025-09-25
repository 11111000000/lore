;;; ert-runner.el --- ERT bootstrap for Lore tests -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory (or load-file-name buffer-file-name))))

;; In batch test environments some setups mark *Messages* read-only.
;; Make all `message' calls safe and redirect output to stdout instead.
(when noninteractive
  (setq message-log-max nil)
  (defun lore-test--safe-message-around (orig fmt &rest args)
    (let ((s (apply #'format fmt args)))
      (condition-case _err
          (princ (concat s "\n"))
        (error nil))
      ;; Do not call `orig' in batch to avoid touching *Messages*.
      s))
  (advice-add 'message :around #'lore-test--safe-message-around))

;; Load umbrella (pulls all)
(require 'lore)

(require 'ert)

;; Load individual test files
(dolist (f (directory-files (file-name-directory (or load-file-name buffer-file-name)) t "\\-test\\.el\\'"))
  (load f nil t))

(ert-run-tests-batch-and-exit)
