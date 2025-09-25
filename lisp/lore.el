;;; lore.el --- Lore umbrella -*- lexical-binding: t; -*-
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://github.com/11111000000/lore.el
;; SPDX-License-Identifier: MIT
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, convenience, search, knowledge

;;; Commentary:
;; Umbrella module: load core/view and built-in getters.

;;; Code:

(eval-and-compile
  (add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name))))

(require 'lore-log)
(require 'lore-events)
(require 'lore-model)
(require 'lore-cache)
(require 'lore-core)
(require 'lore-render)
(require 'lore-view)
(require 'lore-transient nil t)
(require 'lore-integration-context nil t)

;; Built-ins
(require 'lore-getter-elisp)
(require 'lore-getter-grep)
(require 'lore-getter-org)
(require 'lore-getter-info)
(require 'lore-getter-man)
;; Optional web getters (disabled by default)
(require 'lore-getter-web-devdocs nil t)
(require 'lore-getter-web-mdn nil t)

;;;###autoload
(defun lore-version ()
  "Return Lore version string."
  "0.1.0")

(provide 'lore)

;; Local Variables:
;; coding: utf-8
;; End:

;;; lore.el ends here
