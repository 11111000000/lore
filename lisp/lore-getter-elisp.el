;;; lore-getter-elisp.el --- Elisp doc/symbol getter for Lore -*- lexical-binding: t; -*-

;;; Commentary:
;; Synchronous getter using apropos to find symbols and docstrings.

;;; Code:

(eval-and-compile
  (add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name))))

(require 'cl-lib)
(require 'gv)
(require 'lore-core)
(require 'lore-model)
(require 'lore-log)

(defun lore--symbol-doc (sym)
  (or (and (fboundp sym) (documentation sym t))
      (and (boundp sym) (documentation-property sym 'variable-documentation t))
      ""))

(defun lore--symbol-kind (sym)
  (cond
   ((fboundp sym) 'function)
   ((boundp sym) 'variable)
   (t 'symbol)))

(defun lore--score-symbol (sym keywords)
  "Simple scoring: name match weight + doc occurrences."
  (let* ((name (symbol-name sym))
         (doc (lore--symbol-doc sym))
         (base (cl-reduce #'+
                          (mapcar (lambda (kw)
                                    (+ (if (string= name kw) 3 0)
                                       (if (string-match-p (regexp-quote kw) name) 2 0)
                                       (if (and doc (string-match-p (regexp-quote kw) doc)) 1 0)))
                                  keywords)
                          :initial-value 0)))
    (float base)))

(cl-defun lore-getter-elisp (&key request topk &allow-other-keys)
  "Getter: search elisp symbols by keywords in REQUEST. Returns list<lore-result>."
  (let* ((kws (or (alist-get :keywords request) '()))
         (limit (or topk 20)))
    (if (null kws)
        '()
      (let* ((pred (lambda (s) (or (fboundp s) (boundp s))))
             (cands (delete-dups
                     (cl-loop for kw in kws
                              append (apropos-internal kw pred))))
             (scored (cl-loop for s in cands
                              for sc = (lore--score-symbol s kws)
                              when (> sc 0)
                              collect (cons s sc)))
             (sorted (cl-subseq (cl-sort scored #'>
                                         :key #'cdr)
                                0 (min (length scored) limit))))
        (cl-loop for (sym . sc) in sorted
                 for kind = (lore--symbol-kind sym)
                 for doc = (lore--symbol-doc sym)
                 collect
                 (lore-result-create
                  :type 'symbol
                  :title (format "%s" sym)
                  :snippet (or (and doc (car (split-string doc "\n")))
                               (format "%s" sym))
                  :content doc
                  :score sc
                  :source 'elisp
                  :meta (list :name sym :kind kind)))))))

;; Register on load
(lore-register-getter
 'elisp
 :capabilities '(:domains (elisp) :kinds (symbol doc) :match (keyword))
 :fn #'lore-getter-elisp
 :cost 1.0
 :batch-p nil)

(provide 'lore-getter-elisp)
;;; lore-getter-elisp.el ends here
