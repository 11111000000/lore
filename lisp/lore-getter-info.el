;;; lore-getter-info.el --- Info apropos getter for Lore.el  -*- lexical-binding: t; -*-
;; Author: Lore.el
;; Keywords: info, docs, tools
;; Package-Requires: ((emacs "27.1"))
;; URL: https://example.invalid/lore
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Asynchronous getter using external `info --apropos' to search Info manuals.
;; - Streams partial results via emit; finalizes with done.
;; - Produces lore-result of type 'doc with meta (:file :node :desc).
;;
;; Contract:
;;   (lore-getter-info-run &key request topk emit done)
;;
;; Parsing strategy:
;;   We try to match common GNU info apropos output patterns, e.g.
;;     (emacs) Buffers: Node about buffers...
;;     (libc) printf: Formatted output...
;;   Fallbacks are handled gracefully.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'lore-model nil t)
(require 'lore-core  nil t)

(defgroup lore-getter-info nil
  "Info-based getter for Lore.el."
  :group 'lore)

(defcustom lore-info-program "info"
  "Program used to access GNU Info."
  :type 'string
  :group 'lore-getter-info)

(defcustom lore-info-extra-args '("--apropos")
  "Extra arguments for `info'."
  :type '(repeat string)
  :group 'lore-getter-info)

(defcustom lore-info-force-locale t
  "When non-nil, force LC_ALL=LANG=C for `info --apropos' to stabilize output for parsing."
  :type 'boolean
  :group 'lore-getter-info)

(defvar lore-getter-info--state (make-hash-table :test #'eq)
  "Internal state: process → plist(:buf BUF :emit FN :done FN :limit INT :emitted INT :acc LIST).")

(defvar lore-getter-info--tok 0
  "Monotonic token counter for info getter.")

(defun lore-getter-info--pattern (keywords)
  "Build regex pattern for KEYWORDS (join with \".*\")."
  (cond
   ((null keywords) "")
   ((= (length keywords) 1) (car keywords))
   (t (mapconcat #'identity keywords ".*"))))

(defun lore-getter-info--parse-line (line)
  "Parse info apropos LINE. Return (FILE NODE DESC) or nil."
  ;; Common patterns:
  ;; 1) (manual) Node: desc
  ;; 2) manual: Node: desc
  ;; 3) "\"(manual)Node\" -- desc" (seen on some distros, incl. Eglot manual)
  (cond
   ;; Quoted form: "\"(manual)Node\" -- desc"
   ((string-match "\\`\"(\\([^()]+\\))\\([^\"\n]+\\)\"\\s-*--\\s-*\\(.*\\)\\'" line)
    (list (string-trim (match-string 1 line))
          (string-trim (match-string 2 line))
          (string-trim (match-string 3 line))))
   ;; (manual) Node: desc
   ((string-match "\\`(\\([^()]+\\))\\s-*\\([^:]+\\):\\s-*\\(.*\\)\\'" line)
    (list (string-trim (match-string 1 line))
          (string-trim (match-string 2 line))
          (string-trim (match-string 3 line))))
   ;; manual: Node: desc
   ((string-match "\\`\\([^:]+\\):\\s-*\\([^:]+\\):\\s-*\\(.*\\)\\'" line)
    (list (string-trim (match-string 1 line))
          (string-trim (match-string 2 line))
          (string-trim (match-string 3 line))))
   (t nil)))

(defun lore-getter-info--make-result (file node desc score)
  "Create lore-result for FILE manual, NODE, DESC with SCORE."
  (if (fboundp 'lore-result-create)
      (lore-result-create
       :type 'doc
       :title (format "(%s) %s" file node)
       :snippet desc
       :content nil
       :path nil
       :url nil
       :buffer nil
       :beg nil :end nil
       :score (float score)
       :source 'info
       :meta (list :file file :node node :desc desc))
    `((type . doc)
      (title . ,(format "(%s) %s" file node))
      (snippet . ,desc)
      (content . nil)
      (path . nil)
      (url . nil)
      (buffer . nil)
      (beg . nil) (end . nil)
      (score . ,(float score))
      (source . info)
      (meta . ,(list :file file :node node :desc desc)))))

(defun lore-getter-info--emit-batch (proc)
  "Emit accumulated results for PROC."
  (let* ((st (gethash proc lore-getter-info--state))
         (emit (plist-get st :emit))
         (acc (plist-get st :acc)))
    (when (and (functionp emit) acc)
      (funcall emit (nreverse acc))
      (plist-put st :acc nil)
      (puthash proc st lore-getter-info--state))))

(defun lore-getter-info--insert-chunk (buf chunk)
  "Append CHUNK to BUF at end."
  (with-current-buffer buf
    (goto-char (point-max))
    (insert chunk)))

(defun lore-getter-info--score (emitted)
  "Compute score based on number of EMITTED results."
  (max 0.0 (- 1.0 (* 0.03 emitted))))

(defun lore-getter-info--maybe-emit (emit acc emitted)
  "Maybe emit ACC via EMIT every 10 EMITTED results. Return possibly reset ACC."
  (when (and (functionp emit) (zerop (% emitted 10)))
    (funcall emit (nreverse acc))
    (setq acc nil))
  acc)

(defun lore-getter-info--process-line (proc emit limit acc emitted parsed raw-sample tr)
  "Process one TR line. Return plist with updated counters and ACC."
  (if-let ((p (lore-getter-info--parse-line tr)))
      (pcase-let ((`(,file ,node ,desc) p))
        (let* ((score (lore-getter-info--score emitted))
               (acc (cons (lore-getter-info--make-result file node desc score) acc))
               (emitted (1+ emitted))
               (parsed (1+ parsed)))
          (setq acc (lore-getter-info--maybe-emit emit acc emitted))
          (when (>= emitted limit)
            (when (process-live-p proc)
              (delete-process proc)))
          (list :acc acc :emitted emitted :parsed parsed :raw-sample raw-sample)))
    (when (< (length raw-sample) 5)
      (push tr raw-sample))
    (list :acc acc :emitted emitted :parsed parsed :raw-sample raw-sample)))

(defun lore-getter-info--persist-state (proc st acc emitted lines parsed raw-sample)
  "Persist state fields back into ST and update hash for PROC."
  (plist-put st :emitted emitted)
  (plist-put st :acc acc)
  (plist-put st :lines lines)
  (plist-put st :parsed parsed)
  (plist-put st :raw-sample raw-sample)
  (puthash proc st lore-getter-info--state))

(defun lore-getter-info--filter (proc chunk)
  "Process filter for PROC handling CHUNK."
  (let ((st (gethash proc lore-getter-info--state)))
    (when st
      (let* ((buf (plist-get st :buf))
             (limit (or (plist-get st :limit) most-positive-fixnum))
             (emit (plist-get st :emit))
             (emitted (or (plist-get st :emitted) 0))
             (acc (plist-get st :acc))
             (lines (or (plist-get st :lines) 0))
             (parsed (or (plist-get st :parsed) 0))
             (raw-sample (or (plist-get st :raw-sample) nil)))
        (lore-getter-info--insert-chunk buf chunk)
        (with-current-buffer buf
          (goto-char (point-min))
          (while (search-forward "\n" nil t)
            (let* ((line (buffer-substring-no-properties (point-min) (1- (point))))
                   (tr (string-trim-right line))
                   (res (lore-getter-info--process-line proc emit limit acc emitted parsed raw-sample tr)))
              (setq lines (1+ lines))
              (delete-region (point-min) (point))
              (setq acc (plist-get res :acc)
                    emitted (plist-get res :emitted)
                    parsed (plist-get res :parsed)
                    raw-sample (plist-get res :raw-sample)))))
        ;; Persist updated counters while still in scope of let*
        (lore-getter-info--persist-state proc st acc emitted lines parsed raw-sample)))))

(defun lore-getter-info--sentinel (proc event)
  "Finalize PROC on exit."
  (let ((st (gethash proc lore-getter-info--state)))
    (when st
      (unwind-protect
          (progn
            (lore-getter-info--emit-batch proc)
            (let* ((em (or (plist-get st :emitted) 0))
                   (lines (or (plist-get st :lines) 0))
                   (parsed (or (plist-get st :parsed) 0))
                   (sample (or (plist-get st :raw-sample) nil)))
              (lore-log-info "info getter: sentinel=%S; emitted=%d lines=%d parsed=%d"
                             event em lines parsed)
              (when (and (> lines 0) (= parsed 0) sample)
                (lore-log-info "info getter: first lines (unparsed): %S" (nreverse sample))))
            (when (functionp (plist-get st :done))
              (funcall (plist-get st :done) nil)))
        (when-let ((b (plist-get st :buf)))
          (kill-buffer b))
        (remhash proc lore-getter-info--state)))))

;;;###autoload
(cl-defun lore-getter-info-run (&key request topk emit done)
  "Run info apropos getter with REQUEST and TOPK. Stream via EMIT, finalize via DONE."
  (let* ((keywords (or (alist-get :keywords request)
                       (let ((q (alist-get :query request)))
                         (and q (list q)))))
         (pattern (lore-getter-info--pattern keywords))
         (limit   (or topk most-positive-fixnum)))
    (lore-log-info "info getter: start program=%s args=%S pattern=%S scope=%S keywords=%S cwd=%s force-locale=%s"
                   lore-info-program lore-info-extra-args pattern (alist-get :scope request) keywords
                   default-directory lore-info-force-locale)
    (cond
     ((not (executable-find lore-info-program))
      (lore-log-warn "info getter: program not found: %s" lore-info-program)
      (when (functionp done) (funcall done "info-not-found"))
      nil)
     ((or (null keywords) (string-empty-p pattern))
      (lore-log-info "info getter: empty keywords/pattern; early exit")
      (when (functionp done) (funcall done nil))
      nil)
     (t
      (let* ((buf (generate-new-buffer " *lore-info*"))
             ;; Optionally force C locale to stabilize output for parsing
             (process-environment
              (if lore-info-force-locale
                  (append '("LC_ALL=C" "LANG=C") process-environment)
                process-environment))
             (proc (make-process
                    :name "lore-info-apropos"
                    :buffer buf
                    :noquery t
                    :command (append (list lore-info-program) lore-info-extra-args (list pattern))
                    :connection-type 'pipe
                    :filter #'lore-getter-info--filter
                    :sentinel #'lore-getter-info--sentinel)))
        (puthash proc (list :buf buf
                            :emit emit
                            :done done
                            :limit limit
                            :emitted 0
                            :acc nil
                            :lines 0
                            :parsed 0
                            :raw-sample nil
                            :pattern pattern)
                 lore-getter-info--state)
        ;; Handle race where process may exit before state is installed.
        (unless (process-live-p proc)
          (lore-getter-info--sentinel proc "finished"))
        (list :async t
              :token (format "info-%s" (cl-incf lore-getter-info--tok))
              :cancel (lambda ()
                        (when (process-live-p proc)
                          (delete-process proc)))))))))

;; Register getter after core loads
;;;###autoload
(with-eval-after-load 'lore-core
  (when (fboundp 'lore-register-getter)
    (lore-register-getter
     'info
     ;; Treat info as effectively global; also declare project for robust eligibility.
     :capabilities '(:domains (info) :scope (global project) :kinds (doc) :match (keyword regex))
     :fn #'lore-getter-info-run
     :cost 0.8
     :batch-p t)))

(provide 'lore-getter-info)
;;; lore-getter-info.el ends here
