;;; ert-runner.el --- ERT bootstrap for Lore tests -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory (or load-file-name buffer-file-name))))

;; Load umbrella (pulls all)
(require 'lore)

(require 'ert)

;; Load individual test files
(dolist (f (directory-files (file-name-directory (or load-file-name buffer-file-name)) t "\\-test\\.el\\'"))
  (load f nil t))

(ert-run-tests-batch-and-exit)
