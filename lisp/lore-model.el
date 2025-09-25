;;; lore-model.el --- Data model for Lore -*- lexical-binding: t; -*-
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://github.com/11111000000/lore.el
;; SPDX-License-Identifier: MIT
;; Keywords: data, model, tools

;;; Commentary:
;; lore-result struct and pure helpers: key, uniq, normalize, rank.

;;; Code:

(require 'cl-lib)

(cl-defstruct (lore-result (:constructor lore-result-create))
  type     ;; symbol: 'file | 'buffer | 'selection | 'url | 'doc | 'symbol
  title    ;; string
  snippet  ;; string
  content  ;; string | nil
  path     ;; string | nil
  url      ;; string | nil
  buffer   ;; buffer | nil
  beg end  ;; ints | nil
  score    ;; float 0..1
  source   ;; symbol (getter name)
  meta)    ;; plist

(defun lore-result-key (res)
  "Stable key for RES."
  (pcase (lore-result-type res)
    ('file   (format "file:%s" (or (lore-result-path res) "")))
    ('buffer (format "buf:%s:%s"
                     (buffer-name (or (lore-result-buffer res) (current-buffer)))
                     (or (lore-result-path res) "")))
    ('selection (format "sel:%s:%s-%s"
                        (or (lore-result-path res) "")
                        (or (lore-result-beg res) 0)
                        (or (lore-result-end res) 0)))
    ('url    (format "url:%s" (or (lore-result-url res) "")))
    ('symbol (format "sym:%s:%s"
                     (or (plist-get (lore-result-meta res) :name)
                         (lore-result-title res))
                     (or (plist-get (lore-result-meta res) :lib) "")))
    (_       (format "doc:%s"
                     (or (plist-get (lore-result-meta res) :id)
                         (lore-result-title res))))))

(defun lore-uniq (results)
  "Deduplicate RESULTS by key, last occurrence wins."
  (let ((tbl (make-hash-table :test 'equal))
        out)
    (dolist (r results)
      (puthash (lore-result-key r) r tbl))
    (maphash (lambda (_ v) (push v out)) tbl)
    out))

(defun lore-normalize-scores (results)
  "Normalize scores to 0..1; return RESULTS with updated scores."
  (let* ((vals (mapcar (lambda (r) (or (lore-result-score r) 0.0)) results))
         (mx (or (apply #'max 0.0 vals) 1.0)))
    (dolist (r results results)
      (let ((s (or (lore-result-score r) 0.0)))
        (setf (lore-result-score r)
              (if (> mx 0.0) (/ s mx) 0.0))))))

(defun lore-rank (results &optional _secondary)
  "Sort RESULTS by score desc, then title."
  (cl-stable-sort (copy-sequence results)
                  (lambda (a b)
                    (let ((sa (or (lore-result-score a) 0.0))
                          (sb (or (lore-result-score b) 0.0)))
                      (if (/= sa sb)
                          (> sa sb)
                        (string-lessp (or (lore-result-title a) "")
                                      (or (lore-result-title b) "")))))))

(provide 'lore-model)
;;; lore-model.el ends here
