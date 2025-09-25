;;; lore-web.el --- Common helpers for web getters -*- lexical-binding: t; -*-
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://github.com/11111000000/lore.el
;; SPDX-License-Identifier: MIT
;; Keywords: web, privacy, tools

;;; Commentary:
;; Shared privacy confirmation for web getters (MDN/DevDocs/etc.).

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup lore-web nil
  "Common helpers/settings for Lore web getters."
  :group 'lore)

(defcustom lore-web-confirm-function #'yes-or-no-p
  "Function used to confirm privacy-sensitive network queries.
Called with a single MESSAGE argument; should return non-nil when approved."
  :type 'function
  :group 'lore-web)

(defun lore-web-confirm (keywords provider confirm-flag)
  "Return non-nil if allowed to send KEYWORDS for PROVIDER when CONFIRM-FLAG is non-nil.
If CONFIRM-FLAG is nil, always return t (no confirmation)."
  (if (not confirm-flag)
      t
    (let* ((q (mapconcat #'identity (or keywords '()) " "))
           (msg (format "Send query to %s: %s ?" (symbol-name provider) q)))
      (funcall lore-web-confirm-function msg))))

(provide 'lore-web)
;;; lore-web.el ends here
