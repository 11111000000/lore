;;; lore-events.el --- Simple event bus for Lore -*- lexical-binding: t; -*-
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://github.com/11111000000/lore.el
;; SPDX-License-Identifier: MIT
;; Keywords: events, bus, tools

;;; Commentary:
;; Tiny pub/sub for internal decoupling. Debounce is minimal.

;;; Code:

(require 'cl-lib)
(require 'lore-log)

(defvar lore--event-subs (make-hash-table :test 'eq)
  "Hash of topic -> list of subscriber functions.")

(defun lore-events-subscribe (topic fn)
  "Subscribe FN to TOPIC.
FN is called as (FN &rest args)."
  (let ((lst (gethash topic lore--event-subs)))
    (puthash topic (cons fn lst) lore--event-subs))
  t)

(defun lore-events-unsubscribe (topic fn)
  "Unsubscribe FN from TOPIC."
  (let ((lst (gethash topic lore--event-subs)))
    (when lst
      (puthash topic (delq fn lst) lore--event-subs)
      t)))

(defun lore-events-publish (topic &rest args)
  "Publish event TOPIC with ARGS."
  (let ((subs (copy-sequence (gethash topic lore--event-subs))))
    (dolist (fn subs)
      (condition-case err
          (apply fn args)
        (error
         (lore-log-warn "event %S handler error: %S" topic err))))))

;; minimal debounce keyed by (topic . key)
(defvar lore--event-debounce (make-hash-table :test 'equal))

(defun lore-events-debounce (topic key delay fn)
  "Debounce FN for TOPIC/KEY by DELAY seconds."
  (let* ((k (cons topic key))
         (old (gethash k lore--event-debounce)))
    (when (timerp old) (cancel-timer old))
    (puthash k (run-at-time delay nil
                            (lambda ()
                              (remhash k lore--event-debounce)
                              (condition-case err
                                  (funcall fn)
                                (error
                                 (lore-log-warn "debounce %S error: %S" k err)))))
             lore--event-debounce))
  t)

(provide 'lore-events)
;;; lore-events.el ends here
