;;; lore-cache.el --- Simple TTL cache for Lore -*- lexical-binding: t; -*-
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://github.com/11111000000/lore.el
;; SPDX-License-Identifier: MIT
;; Keywords: cache, ttl, tools

;;; Commentary:
;; In-memory cache with TTL seconds.

;;; Code:

(require 'cl-lib)

(defcustom lore-cache-enabled t
  "When non-nil, enable Lore cache for request results."
  :type 'boolean
  :group 'lore)

(defcustom lore-cache-ttl 60.0
  "Default TTL for Lore cache entries in seconds."
  :type 'number
  :group 'lore)

(defvar lore--cache (make-hash-table :test 'equal))
(defvar lore--cache-time (make-hash-table :test 'equal))

(defun lore-cache-get (key)
  "Get cached value by KEY, or nil if expired/missing."
  (let ((exp (gethash key lore--cache-time)))
    (when (and exp (> exp (float-time)))
      (gethash key lore--cache))))

(defun lore-cache-put (key val &optional ttl)
  "Put VAL by KEY with TTL seconds."
  (puthash key val lore--cache)
  (puthash key (+ (float-time) (or ttl lore-cache-ttl)) lore--cache-time)
  val)

(defun lore-request-fingerprint (req)
  "Canonical fingerprint string for request REQ (alist)."
  (let ((fields (list :query :keywords :intent :filters :targets :scope :max-k)))
    (prin1-to-string
     (mapcar (lambda (k) (cons k (alist-get k req)))
             fields))))

(provide 'lore-cache)
;;; lore-cache.el ends here
