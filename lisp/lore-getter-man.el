;;; lore-getter-man.el --- man -k getter for Lore.el  -*- lexical-binding: t; -*-
;; Author: Lore.el
;; Keywords: unix, man, docs
;; Package-Requires: ((emacs "27.1"))
;; URL: https://example.invalid/lore
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Asynchronous getter using `man -k' (apropos) to find manpages.
;; - Streams partial results via emit; finalizes with done.
;; - Produces lore-result of type 'doc with meta (:page :section :desc :name).

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'lore-model nil t)
(require 'lore-core  nil t)

(defgroup lore-getter-man nil
  "Manpage getter for Lore.el."
  :group 'lore)

(defcustom lore-man-program "man"
  "Program used to access UNIX manual pages."
  :type 'string
  :group 'lore-getter-man)

(defcustom lore-man-extra-args '("-k")
  "Extra arguments for `man'."
  :type '(repeat string)
  :group 'lore-getter-man)

(defcustom lore-man-force-locale t
  "When non-nil, force LC_ALL=LANG=C for `man -k' to stabilize output for parsing."
  :type 'boolean
  :group 'lore-getter-man)

(defvar lore-getter-man--state (make-hash-table :test #'eq)
  "Internal state: process → plist(:buf BUF :emit FN :done FN :limit INT :emitted INT :acc LIST).")

(defvar lore-getter-man--tok 0
  "Monotonic token counter for man getter.")

(defun lore-getter-man--pattern (keywords)
  "Build pattern for man -k from KEYWORDS."
  (cond
   ((null keywords) "")
   ((= (length keywords) 1) (car keywords))
   (t (mapconcat #'identity keywords " "))))

(defun lore-getter-man--parse-line (line)
  "Parse man -k LINE. Return (NAME SECTION DESC) or nil.
Expected forms:
  name (3) - description
  name (3posix) - description"
  (when (string-match "\\`\\([^[:space:]]+\\)\\s-*(\\([^)]+\\))\\s-*[-–—]\\s-*\\(.*\\)\\'" line)
    (list (match-string 1 line)
          (match-string 2 line)
          (string-trim (match-string 3 line)))))

(defun lore-getter-man--make-result (name section desc score)
  "Create lore-result for man NAME(SECTION) with DESC and SCORE."
  (let ((page (if (and section (not (string-empty-p section)))
                  (format "%s(%s)" name section)
                name)))
    (if (fboundp 'lore-result-create)
        (lore-result-create
         :type 'doc
         :title page
         :snippet desc
         :content nil
         :path nil
         :url nil
         :buffer nil
         :beg nil :end nil
         :score (float score)
         :source 'man
         :meta (list :page page :name name :section section :desc desc))
      `((type . doc)
        (title . ,page)
        (snippet . ,desc)
        (content . nil)
        (path . nil)
        (url . nil)
        (buffer . nil)
        (beg . nil) (end . nil)
        (score . ,(float score))
        (source . man)
        (meta . ,(list :page page :name name :section section :desc desc))))))

(defun lore-getter-man--emit-batch (proc)
  "Emit accumulated results for PROC."
  (let* ((st (gethash proc lore-getter-man--state))
         (emit (plist-get st :emit))
         (acc (plist-get st :acc)))
    (when (and (functionp emit) acc)
      (funcall emit (nreverse acc))
      (plist-put st :acc nil)
      (puthash proc st lore-getter-man--state))))

(defun lore-getter-man--insert-chunk (buf chunk)
  "Append CHUNK to BUF at end."
  (with-current-buffer buf
    (goto-char (point-max))
    (insert chunk)))

(defun lore-getter-man--score (emitted)
  "Compute score based on number of EMITTED results."
  (max 0.0 (- 1.0 (* 0.03 emitted))))

(defun lore-getter-man--maybe-emit (emit acc emitted)
  "Maybe emit ACC via EMIT every 10 EMITTED results. Return possibly reset ACC."
  (when (and (functionp emit) (zerop (% emitted 10)))
    (funcall emit (nreverse acc))
    (setq acc nil))
  acc)

(defun lore-getter-man--process-line (proc emit limit acc emitted parsed raw-sample trim)
  "Process one TRIM line. Return plist with updated counters and ACC."
  (if-let ((p (lore-getter-man--parse-line trim)))
      (pcase-let ((`(,name ,section ,desc) p))
        (let* ((score (lore-getter-man--score emitted))
               (acc (cons (lore-getter-man--make-result name section desc score) acc))
               (emitted (1+ emitted))
               (parsed (1+ parsed)))
          (setq acc (lore-getter-man--maybe-emit emit acc emitted))
          (when (>= emitted limit)
            (when (process-live-p proc)
              (delete-process proc)))
          (list :acc acc :emitted emitted :parsed parsed :raw-sample raw-sample)))
    ;; keep a small sample of unparsed lines for diagnostics
    (when (< (length raw-sample) 5)
      (push trim raw-sample))
    (list :acc acc :emitted emitted :parsed parsed :raw-sample raw-sample)))

(defun lore-getter-man--persist-state (proc st acc emitted lines parsed raw-sample)
  "Persist state fields back into ST and update hash for PROC."
  (plist-put st :emitted emitted)
  (plist-put st :acc acc)
  (plist-put st :lines lines)
  (plist-put st :parsed parsed)
  (plist-put st :raw-sample raw-sample)
  (puthash proc st lore-getter-man--state))

(defun lore-getter-man--filter (proc chunk)
  "Process filter for PROC handling CHUNK."
  (let ((st (gethash proc lore-getter-man--state)))
    (when st
      (let* ((buf (plist-get st :buf))
             (limit (or (plist-get st :limit) most-positive-fixnum))
             (emit (plist-get st :emit))
             (emitted (or (plist-get st :emitted) 0))
             (acc (plist-get st :acc))
             (lines (or (plist-get st :lines) 0))
             (parsed (or (plist-get st :parsed) 0))
             (raw-sample (or (plist-get st :raw-sample) nil)))
        (lore-getter-man--insert-chunk buf chunk)
        (with-current-buffer buf
          (goto-char (point-min))
          (while (search-forward "\n" nil t)
            (let* ((line (buffer-substring-no-properties (point-min) (1- (point))))
                   (trim (string-trim-right line))
                   (res (lore-getter-man--process-line proc emit limit acc emitted parsed raw-sample trim)))
              (setq lines (1+ lines))
              (delete-region (point-min) (point))
              (setq acc (plist-get res :acc)
                    emitted (plist-get res :emitted)
                    parsed (plist-get res :parsed)
                    raw-sample (plist-get res :raw-sample)))))
        ;; Persist updated counters while still in scope of let*
        (lore-getter-man--persist-state proc st acc emitted lines parsed raw-sample)))))

(defun lore-getter-man--sentinel (proc event)
  "Finalize PROC on exit."
  (let ((st (gethash proc lore-getter-man--state)))
    (when st
      (unwind-protect
          (progn
            ;; Flush any accumulated batch first
            (lore-getter-man--emit-batch proc)
            (let* ((em (or (plist-get st :emitted) 0))
                   (lines (or (plist-get st :lines) 0))
                   (parsed (or (plist-get st :parsed) 0))
                   (sample (or (plist-get st :raw-sample) nil)))
              (lore-log-info "man getter: sentinel=%S; emitted=%d lines=%d parsed=%d"
                             event em lines parsed)
              (when (and (> lines 0) (= parsed 0) sample)
                (lore-log-info "man getter: first lines (unparsed): %S" (nreverse sample)))
              ;; Fallback: if -k produced no parseable lines, try direct page existence with `man -w`
              (when (and (= parsed 0))
                (let* ((pat (plist-get st :pattern))
                       (emit (plist-get st :emit)))
                  (when (and (stringp pat) (not (string-empty-p pat)))
                    (let* ((process-environment
                            (if lore-man-force-locale
                                (append '("LC_ALL=C" "LANG=C") process-environment)
                              process-environment))
                           (out (with-temp-buffer
                                  (let ((status (call-process lore-man-program nil t nil "-w" pat)))
                                    (list status (buffer-string))))))
                      (pcase-let ((`(,stcode ,body) out))
                        (lore-log-info "man getter: fallback `man -w %s` status=%s" pat stcode)
                        (when (and (numberp stcode) (= stcode 0) (stringp body))
                          (let* ((first (car (split-string body "[\r\n]+" t)))
                                 (section (cond
                                           ((and first (string-match "/man\\([0-9A-Za-z]+\\)/" first))
                                            (match-string 1 first))
                                           ((and first (string-match "\\.\\([0-9A-Za-z]+\\)\\(\\.gz\\)?\\'" first))
                                            (match-string 1 first))
                                           (t nil)))
                                 (desc (format "man page for %s" pat))
                                 (res  (list (lore-getter-man--make-result pat section desc 1.0))))
                            (when (functionp emit)
                              (lore-log-info "man getter: fallback emitted 1 result for %s (section=%s)" pat section)
                              (funcall emit res))))))))))
            (when (functionp (plist-get st :done))
              (funcall (plist-get st :done) nil)))
        (when-let ((b (plist-get st :buf)))
          (kill-buffer b))
        (remhash proc lore-getter-man--state)))))

;;;###autoload
(cl-defun lore-getter-man-run (&key request topk emit done)
  "Run man -k getter with REQUEST and TOPK. Stream via EMIT, finalize via DONE."
  (let* ((keywords (or (alist-get :keywords request)
                       (let ((q (alist-get :query request)))
                         (and q (list q)))))
         (pattern (lore-getter-man--pattern keywords))
         (limit   (or topk most-positive-fixnum)))
    (lore-log-info "man getter: start program=%s args=%S pattern=%S scope=%S keywords=%S cwd=%s force-locale=%s"
                   lore-man-program lore-man-extra-args pattern (alist-get :scope request) keywords
                   default-directory lore-man-force-locale)
    (cond
     ((not (executable-find lore-man-program))
      (lore-log-warn "man getter: program not found: %s" lore-man-program)
      (when (functionp done) (funcall done "man-not-found"))
      nil)
     ((or (null keywords) (string-empty-p pattern))
      (lore-log-info "man getter: empty keywords/pattern; early exit")
      (when (functionp done) (funcall done nil))
      nil)
     (t
      (let* ((buf (generate-new-buffer " *lore-man*"))
             ;; Optionally force C locale to stabilize output for parsing
             (process-environment
              (if lore-man-force-locale
                  (append '("LC_ALL=C" "LANG=C") process-environment)
                process-environment))
             (proc (make-process
                    :name "lore-man-k"
                    :buffer buf
                    :noquery t
                    :command (append (list lore-man-program) lore-man-extra-args (list pattern))
                    :connection-type 'pipe
                    :filter #'lore-getter-man--filter
                    :sentinel #'lore-getter-man--sentinel)))
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
                 lore-getter-man--state)
        ;; Handle race where process may exit before state is installed.
        (unless (process-live-p proc)
          (lore-getter-man--sentinel proc "finished"))
        (list :async t
              :token (format "man-%s" (cl-incf lore-getter-man--tok))
              :cancel (lambda ()
                        (when (process-live-p proc)
                          (delete-process proc)))))))))

;; Register getter after core loads
;;;###autoload
(with-eval-after-load 'lore-core
  (when (fboundp 'lore-register-getter)
    (lore-register-getter
     'man
     ;; Treat man as effectively global; also declare project for robust eligibility.
     :capabilities '(:domains (man) :scope (global project) :kinds (doc) :match (keyword regex))
     :fn #'lore-getter-man-run
     :cost 0.9
     :batch-p t)))

(provide 'lore-getter-man)
;;; lore-getter-man.el ends here
