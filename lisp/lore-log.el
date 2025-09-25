;;; lore-log.el --- Lightweight logging for Lore -*- lexical-binding: t; -*-
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://github.com/11111000000/lore.el
;; SPDX-License-Identifier: MIT
;; Keywords: logging, tools, debugging

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
  "Core log function. Robust in batch/noninteractive and read-only *Messages*."
  (when (lore--should-log-p level)
    (let ((msg (apply #'format fmt args)))
      (if noninteractive
          ;; In batch ERT or noninteractive sessions, avoid touching *Messages* entirely.
          (condition-case _err
              (progn
                (princ (format "[lore:%s] %s\n" level msg)))
            (error nil))
        ;; Interactive Emacs: try `message', fall back to direct buffer insert.
        (condition-case _err
            (message "[lore:%s] %s" level msg)
          (error
           ;; Fallback: append directly to *Messages* ignoring read-only.
           (when-let ((buf (get-buffer "*Messages*")))
             (with-current-buffer buf
               (let ((inhibit-read-only t)
                     (inhibit-modification-hooks t)
                     (buffer-undo-list t))
                 (condition-case _err2
                     (progn
                       (goto-char (point-max))
                       (insert (format "[lore:%s] %s\n" level msg)))
                   (error nil)))))))))))

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
