;;; lore-log.el --- Lightweight logging for Lore -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: az
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Minimal logger with leveled output. Formatting deferred unless enabled.

;;; Code:

(defgroup lore nil
  "Lore.el — universal retrieval framework."
  :group 'tools)

(defcustom lore-log-level 'warn
  "Logging level for Lore.
One of: nil, error, warn, info, debug."
  :type '(choice (const :tag "Off" nil)
                 (const error) (const warn)
                 (const info) (const debug))
  :group 'lore)

(defun lore--log-level->num (level)
  (pcase level
    ('error 10)
    ('warn  20)
    ('info  30)
    ('debug 40)
    (_ 100))) ; off

(defun lore--should-log-p (level)
  (and lore-log-level
       (<= (lore--log-level->num level)
           (lore--log-level->num lore-log-level))))

(defun lore--log (level fmt &rest args)
  "Core log function."
  (when (lore--should-log-p level)
    (let ((msg (apply #'format fmt args)))
      (message "[lore:%s] %s" level msg))))

(defun lore-log-error (fmt &rest args)
  (apply #'lore--log 'error fmt args))

(defun lore-log-warn (fmt &rest args)
  (apply #'lore--log 'warn fmt args))

(defun lore-log-info (fmt &rest args)
  (apply #'lore--log 'info fmt args))

(defun lore-log-debug (fmt &rest args)
  (apply #'lore--log 'debug fmt args))

(provide 'lore-log)
;;; lore-log.el ends here
