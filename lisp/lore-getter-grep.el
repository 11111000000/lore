;;; lore-getter-grep.el --- Ripgrep-based getter for Lore.el  -*- lexical-binding: t; -*-
;; Author: Lore.el
;; Keywords: convenience, search, tools
;; Package-Requires: ((emacs "27.1"))
;; URL: https://example.invalid/lore
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Asynchronous project grep getter for Lore.el.
;; - Uses ripgrep (rg) if available.
;; - Streams partial results via `emit', finalizes via `done'.
;; - Registers itself with Lore core on load.
;;
;; Contract of getter:
;;   (lore-getter-grep-run &key request topk emit done) → list<lore-result> | (:async t :token TOKEN :cancel FN)
;;
;; Result `meta' fields:
;;   :line (int) :col (int) :text (string)

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Optional require to register the getter after lore-core is available.
(require 'lore-model nil t)
(require 'lore-core  nil t)

(defgroup lore-getter-grep nil
  "Ripgrep-based getter for Lore.el."
  :group 'lore)

(defcustom lore-grep-program "rg"
  "Program used for code/text search."
  :type 'string
  :group 'lore-getter-grep)

(defcustom lore-grep-extra-args
  '("--smart-case" "--line-number" "--column" "--color=never" "--no-heading" "--no-config")
  "Extra arguments for ripgrep."
  :type '(repeat string)
  :group 'lore-getter-grep)

(defcustom lore-grep-ignore-globs
  '("!node_modules/*" "!.git/*" "!.cache/*")
  "Glob patterns to ignore in search. Each entry is passed as \"--glob PATTERN\"."
  :type '(repeat string)
  :group 'lore-getter-grep)

(defcustom lore-grep-max-count-factor 5
  "Factor to multiply topk for --max-count to avoid dropping too early."
  :type 'integer
  :group 'lore-getter-grep)

(defvar lore-getter-grep--state (make-hash-table :test #'eq)
  "Internal state: process → plist(:buf BUF :emit FN :done FN :limit INT :emitted INT :acc LIST).")

(defvar lore-getter-grep--counter 0
  "Internal counter for unique tokens.")

(defun lore-getter-grep--project-root ()
  "Detect project root or return `default-directory'."
  (cond
   ((fboundp 'project-current)
    (when-let ((pr (project-current nil)))
      (car (project-roots pr))))
   ((fboundp 'vc-root-dir)
    (or (vc-root-dir) default-directory))
   (t default-directory)))

(defun lore-getter-grep--build-args (keywords topk)
  "Build ripgrep args from KEYWORDS and TOPK."
  (let* ((pattern
          (cond
           ((null keywords) "")
           ((= (length keywords) 1) (car keywords))
           (t (mapconcat #'identity keywords ".*"))))
         (glob-args (cl-mapcan (lambda (g) (list "--glob" g)) lore-grep-ignore-globs)))
    (append lore-grep-extra-args
            glob-args
            (when topk
              (list "--max-count" (number-to-string (max 1 (* lore-grep-max-count-factor topk)))))
            (list pattern))))

(defun lore-getter-grep--parse-line (line)
  "Parse ripgrep LINE in form path:line:col:text."
  (when (string-match "\\`\\([^:\n]+\\):\\([0-9]+\\):\\([0-9]+\\):\\(.*\\)\\'" line)
    (let ((path (match-string 1 line))
          (ln   (string-to-number (match-string 2 line)))
          (col  (string-to-number (match-string 3 line)))
          (txt  (match-string 4 line)))
      (list path ln col txt))))

(defun lore-getter-grep--make-result (path line col text score)
  "Create a lore-result for PATH at LINE:COL with TEXT and SCORE."
  (let ((abs (expand-file-name path)))
    (if (fboundp 'lore-result-create)
        (lore-result-create
         :type 'file
         :title (file-name-nondirectory abs)
         :snippet text
         :content nil
         :path abs
         :url nil
         :buffer nil
         :beg nil
         :end nil
         :score (float score)
         :source 'grep
         :meta (list :line line :col col :text text))
      ;; Fallback alist if struct not available
      `((type . file)
        (title . ,(file-name-nondirectory abs))
        (snippet . ,text)
        (content . nil)
        (path . ,abs)
        (url . nil)
        (buffer . nil)
        (beg . nil)
        (end . nil)
        (score . ,(float score))
        (source . grep)
        (meta . ,(list :line line :col col :text text))))))

(defun lore-getter-grep--emit-batch (proc)
  "Emit accumulated results batch for PROC."
  (let* ((st (gethash proc lore-getter-grep--state))
         (emit (plist-get st :emit))
         (acc  (plist-get st :acc)))
    (when (and (functionp emit) acc)
      (funcall emit (nreverse acc))
      (plist-put st :acc nil)
      (puthash proc st lore-getter-grep--state))))

(defun lore-getter-grep--filter (proc chunk)
  "Process filter for PROC handling CHUNK."
  (let* ((st (gethash proc lore-getter-grep--state)))
    (when st
      (let* ((buf (plist-get st :buf))
             (emit (plist-get st :emit))
             (limit (or (plist-get st :limit) most-positive-fixnum))
             (emitted (or (plist-get st :emitted) 0))
             (acc (plist-get st :acc)))
        (with-current-buffer buf
          (goto-char (point-max))
          (insert chunk)
          (goto-char (point-min))
          (while (search-forward "\n" nil t)
            (let ((line (buffer-substring-no-properties (point-min) (1- (point)))))
              (delete-region (point-min) (point))
              (when-let ((data (lore-getter-grep--parse-line (string-trim-right line))))
                (pcase-let ((`(,path ,ln ,col ,text) data))
                  (let ((score (max 0.0 (- 1.0 (* 0.03 emitted)))))
                    (push (lore-getter-grep--make-result path ln col text score) acc)
                    (setq emitted (1+ emitted))
                    (when (and (functionp emit) (zerop (% emitted 10)))
                      (funcall emit (nreverse acc))
                      (setq acc nil))
                    (when (>= emitted limit)
                      (when (process-live-p proc)
                        (delete-process proc)))))))))
        (plist-put st :emitted emitted)
        (plist-put st :acc acc)
        (puthash proc st lore-getter-grep--state)))))

(defun lore-getter-grep--sentinel (proc _event)
  "Finalize PROC on exit."
  (let* ((st (gethash proc lore-getter-grep--state)))
    (when st
      (unwind-protect
          (progn
            ;; Cancel timeout timer if any
            (when-let ((tm (plist-get st :timer)))
              (when (timerp tm) (cancel-timer tm)))
            ;; Flush any trailing partial line (no newline at EOF)
            (let* ((buf (plist-get st :buf))
                   (limit (or (plist-get st :limit) most-positive-fixnum))
                   (emitted (or (plist-get st :emitted) 0))
                   (acc (plist-get st :acc)))
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (let ((left (string-trim-right (buffer-substring-no-properties (point-min) (point-max)))))
                    (when (and (> (length left) 0))
                      (when-let ((data (lore-getter-grep--parse-line left)))
                        (pcase-let ((`(,path ,ln ,col ,text) data))
                          (let ((score (max 0.0 (- 1.0 (* 0.03 emitted)))))
                            (push (lore-getter-grep--make-result path ln col text score) acc)
                            (setq emitted (1+ emitted)))))))))
              (plist-put st :emitted emitted)
              (plist-put st :acc acc)
              (puthash proc st lore-getter-grep--state))
            (lore-getter-grep--emit-batch proc)
            (when (functionp (plist-get st :done))
              (funcall (plist-get st :done) nil)))
        (when-let ((buf (plist-get st :buf)))
          (kill-buffer buf))
        (remhash proc lore-getter-grep--state)))))

;;;###autoload
(cl-defun lore-getter-grep-run (&key request topk emit done)
  "Run ripgrep-based getter with REQUEST.
TOPK caps number of items. EMIT receives partial lists. DONE called at end.

Returns list of results (sync) or plist (:async t :token TOKEN :cancel FN)."
  (let* ((keywords (or (alist-get :keywords request)
                       (let ((q (alist-get :query request)))
                         (and q (list q)))))
         (root (or (alist-get :project-root request)
                   (lore-getter-grep--project-root)))
         ;; Respect scope: only run in 'project scope.
         (scope (alist-get :scope request))
         (default-directory (or root default-directory)))
    (cond
     ((not (executable-find lore-grep-program))
      (when (functionp done) (funcall done "rg-not-found"))
      nil)
     ((or (null keywords) (and (eq scope 'project) (not root)))
      (when (functionp done) (funcall done nil))
      nil)
     (t
      (let* ((token (cl-incf lore-getter-grep--counter))
             (buf (generate-new-buffer " *lore-rg*"))
             (workdir (or (alist-get :project-root request) default-directory))
             (proc (let ((default-directory workdir))
                     (make-process
                      :name (format "lore-rg-%d" token)
                      :buffer buf
                      :noquery t
                      :command (cons lore-grep-program
                                     (append (lore-getter-grep--build-args keywords topk)
                                             (list ".")))
                      :connection-type 'pipe
                      :filter #'lore-getter-grep--filter
                      :sentinel #'lore-getter-grep--sentinel)))
             ;; Fallback timeout to ensure finalization even if sentinel doesn't fire promptly
             (timeout-timer (run-at-time 4.5 nil
                                         (lambda ()
                                           (when (gethash proc lore-getter-grep--state)
                                             (when (process-live-p proc)
                                               (delete-process proc)))))))
        (puthash proc (list :buf buf
                            :emit emit
                            :done done
                            :limit (or topk most-positive-fixnum)
                            :emitted 0
                            :acc nil
                            :timer timeout-timer)
                 lore-getter-grep--state)
        ;; Handle race where process may exit before state is installed.
        (unless (process-live-p proc)
          (lore-getter-grep--sentinel proc "finished"))
        (list :async t
              :token token
              :cancel (lambda ()
                        (when (process-live-p proc)
                          (delete-process proc)))))))))

;; Register getter when core is available.
;;;###autoload
(with-eval-after-load 'lore-core
  (when (fboundp 'lore-register-getter)
    (lore-register-getter
     'grep
     :capabilities '(:match (keyword regex)
                            :scope (project global)
                            :kinds (file code)
                            :domains (project))
     :fn #'lore-getter-grep-run
     :cost 0.3
     :batch-p t)))

(provide 'lore-getter-grep)
;;; lore-getter-grep.el ends here
