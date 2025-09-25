;;; lore-transient.el --- Transient UI for Lore -*- lexical-binding: t; -*-

;;; Commentary:
;; Quick controls for scope, top-K and domains, with immediate rerun.
;; Requires `transient'. Falls back to a simple message if unavailable.

;;; Code:

(require 'cl-lib)
(require 'gv)
(require 'subr-x)
(require 'lore-view)
(require 'lore-core)
(require 'lore-render)

(defvar-local lore-tx-scope 'project)
(defvar-local lore-tx-k 20)
(defvar-local lore-tx-domains '(project elisp org info man))

(defun lore-transient--init-state ()
  "Initialize transient state from last request in current buffer."
  (when (boundp 'lore--last-request)
    (let* ((req lore--last-request)
           (scope (or (alist-get :scope req) 'project))
           (k (or (alist-get :max-k req) 20))
           (targets (or (alist-get :targets req) '())))
      (setq lore-tx-scope scope
            lore-tx-k k
            lore-tx-domains (or (and targets (copy-sequence targets))
                                '(project elisp org info man))))))

(defun lore-transient--toggle (dom)
  "Toggle domain DOM in `lore-tx-domains'."
  (if (memq dom lore-tx-domains)
      (setq lore-tx-domains (delq dom lore-tx-domains))
    (push dom lore-tx-domains))
  (setq lore-tx-domains (delete-dups lore-tx-domains)))

(defun lore-transient--domains->prefixes ()
  "Return list of domain prefixes strings for current selection."
  (cl-loop for d in '(project elisp org info man)
           when (memq d lore-tx-domains)
           collect (format "%s:" (symbol-name d))))

(defun lore-transient--filters->tokens (req)
  "Turn REQ :filters into tokens like #tag."
  (let ((filters (alist-get :filters req)))
    (cl-loop for (k . v) in filters
             if (and (eq k :tag) (stringp v))
             collect (concat "#" v))))

(defun lore-transient--base-query (req)
  "Rebuild plain query string from REQ keywords and filters."
  (let* ((kws (mapcar #'identity (or (alist-get :keywords req) '())))
         (filt (lore-transient--filters->tokens req)))
    (string-join (append kws filt) " ")))

(defun lore-transient--build-query (req)
  "Build new query string from REQ and transient state."
  (let* ((prefixes (lore-transient--domains->prefixes))
         (base (lore-transient--base-query req))
         (flags (list (format "?scope=%s" (symbol-name lore-tx-scope))
                      (format "?k=%d" lore-tx-k))))
    (string-trim (string-join (append prefixes (list base) flags) " "))))

(defun lore-transient-run ()
  "Run Lore with current transient settings."
  (interactive)
  (unless (boundp 'lore--last-request)
    (user-error "Run M-x lore-ask first"))
  (let* ((req lore--last-request)
         (q (lore-transient--build-query req)))
    (lore-ask q)))

;; Define transient UI, if available
(if (require 'transient nil t)

    (progn
      (transient-define-prefix lore-transient ()
        "Lore controls"
        [:description (lambda () (lore-transient--init-state)
                        (format "Scope: %s   Top-K: %d   Highlight: %s"
                                lore-tx-scope
                                lore-tx-k
                                (if lore-render-highlight-keywords "on" "off")))]
        [["Scope"
          ("s p" "project" (lambda () (interactive) (setq lore-tx-scope 'project)))
          ("s g" "global"  (lambda () (interactive) (setq lore-tx-scope 'global)))]
         ["Top-K"
          ("k" "set K" (lambda (k)
                         (interactive (list (read-number "Top-K: " lore-tx-k)))
                         (setq lore-tx-k (max 1 (truncate k)))))]
         ["Domains"
          ("d p" "project" (lambda () (interactive) (lore-transient--toggle 'project)))
          ("d e" "elisp"   (lambda () (interactive) (lore-transient--toggle 'elisp)))
          ("d o" "org"     (lambda () (interactive) (lore-transient--toggle 'org)))
          ("d i" "info"    (lambda () (interactive) (lore-transient--toggle 'info)))
          ("d m" "man"     (lambda () (interactive) (lore-transient--toggle 'man)))]
         ["Display"
          ("x h" "toggle highlight"
           (lambda ()
             (interactive)
             (setq lore-render-highlight-keywords (not lore-render-highlight-keywords))
             (message "Lore highlight %s" (if lore-render-highlight-keywords "on" "off"))))]
         ["Weights"
          ("w e" "bias elisp"
           (lambda ()
             (interactive)
             (setq lore-source-weights '((elisp . 1.5) (grep . 1.0) (org . 1.0) (info . 1.0) (man . 1.0)))
             (message "Lore weights: bias elisp")))
          ("w p" "bias project"
           (lambda ()
             (interactive)
             (setq lore-source-weights '((elisp . 1.0) (grep . 1.6) (org . 1.1) (info . 1.0) (man . 1.0)))
             (message "Lore weights: bias project/grep")))
          ("w r" "reset"
           (lambda ()
             (interactive)
             (setq lore-source-weights '((elisp . 1.0) (grep . 1.0) (org . 1.0) (info . 1.0) (man . 1.0)))
             (message "Lore weights reset")))]]
        [["Actions"
          ("RET" "Run" lore-transient-run)
          ("C-c C-c" "Run" lore-transient-run)
          ("q" "Quit" transient-quit-one)]]))

  ;; Fallback when transient is unavailable
  (defun lore-transient ()
    "Transient UI is unavailable. Please install the `transient' package."
    (interactive)
    (user-error "Install package 'transient' to use lore-transient")))

(provide 'lore-transient)
;;; lore-transient.el ends here
