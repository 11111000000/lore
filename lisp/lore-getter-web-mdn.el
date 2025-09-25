;;; lore-getter-web-mdn.el --- MDN web getter for Lore -*- lexical-binding: t; -*-
;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://github.com/11111000000/lore.el
;; SPDX-License-Identifier: MIT
;; Package-Requires: ((emacs "27.1"))
;; Keywords: mdn, web, docs, tools

;;; Commentary:
;; Asynchronous web getter (optional, disabled by default).
;; Uses MDN API-like endpoint (placeholder) to fetch search JSON and convert
;; into Lore url/doc results.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'json)

(require 'lore-model nil t)
(require 'lore-core  nil t)
(require 'lore-web)

(defgroup lore-getter-web-mdn nil
  "Web (MDN) getter for Lore."
  :group 'lore)

(defcustom lore-web-mdn-enabled nil
  "When non-nil, enable MDN web getter."
  :type 'boolean
  :group 'lore-getter-web-mdn)

(defcustom lore-web-mdn-confirm-privacy t
  "When non-nil, ask confirmation before sending query to the web."
  :type 'boolean
  :group 'lore-getter-web-mdn)

(defcustom lore-web-mdn-endpoint "https://example.invalid/mdn/search"
  "Base endpoint for MDN-like search API (placeholder)."
  :type 'string
  :group 'lore-getter-web-mdn)

(defcustom lore-web-mdn-timeout 3.0
  "HTTP timeout in seconds for MDN queries."
  :type 'number
  :group 'lore-getter-web-mdn)

(defun lore-web-mdn--parse-json (json-string)
  "Parse MDN JSON-STRING into a list of (:title :url :summary)."
  (let* ((obj (condition-case _err
                  (json-parse-string json-string :object-type 'alist :array-type 'list :null-object nil)
                (error nil)))
         ;; Common MDN API: { documents: [ { title, mdn_url, summary } ] }
         (docs (or (and (listp obj) (alist-get 'documents obj))
                   (alist-get :documents obj)
                   (alist-get 'results obj)))
         (base (or (alist-get 'base_url obj) "https://developer.mozilla.org")))
    (cl-loop for d in (or docs '())
             for title = (or (alist-get 'title d) (alist-get :title d) "")
             for mdn-url = (or (alist-get 'mdn_url d) (alist-get :mdn_url d)
                               (alist-get 'url d) (alist-get :url d) "")
             for sum = (or (alist-get 'summary d) (alist-get :summary d) "")
             for full = (cond
                         ((string-prefix-p "http" mdn-url) mdn-url)
                         ((string-prefix-p "/" mdn-url) (concat base mdn-url))
                         (t mdn-url))
             when (and (stringp title) (stringp full) (not (string-empty-p full)))
             collect (list :title title :url full :summary (or sum "")))))

(defun lore-web-mdn--items->results (items)
  "Convert parsed ITEMS to lore-result list."
  (let ((score 1.0))
    (cl-loop for it in items
             collect
             (lore-result-create
              :type 'url
              :title (or (plist-get it :title) (plist-get it :url))
              :snippet (or (plist-get it :summary) (plist-get it :url))
              :content nil
              :url (plist-get it :url)
              :score (prog1 score (setq score (max 0.0 (- score 0.02))))
              :source 'mdn
              :meta it))))

(defun lore-web-mdn--build-url (endpoint keywords)
  "Build URL from ENDPOINT and KEYWORDS list."
  (let* ((q (mapconcat #'identity keywords " "))
         (encoded (url-hexify-string q)))
    (format "%s?q=%s&format=json" (string-trim-right endpoint "/") encoded)))

(defun lore-web-mdn--http-fetch (url callback)
  "Fetch URL asynchronously and call CALLBACK with (status body-string)."
  (let ((buf nil) (done nil))
    (url-retrieve
     url
     (lambda (_h)
       (setq buf (current-buffer))
       (let* ((http-status (or (and (boundp 'url-http-response-status) url-http-response-status) 200))
              (body (progn
                      (goto-char (point-min))
                      (when (search-forward "\n\n" nil t)
                        (buffer-substring-no-properties (point) (point-max))))))
         (setq done t)
         (unwind-protect
             (funcall callback http-status (or body ""))
           (when (buffer-live-p buf) (kill-buffer buf)))))
     nil t)
    ;; Timeout guard
    (run-at-time lore-web-mdn-timeout nil
                 (lambda ()
                   (unless done
                     (when (buffer-live-p buf) (kill-buffer buf))
                     (funcall callback 408 ""))))))

;;;###autoload
(cl-defun lore-getter-web-mdn-run (&key request topk emit done)
  "Run MDN web getter with REQUEST, TOPK; stream via EMIT, finalize via DONE."
  (let* ((keywords (or (alist-get :keywords request)
                       (let ((q (alist-get :query request)))
                         (and q (split-string q "[ \t\n]+" t)))))
         (limit (or topk 20)))
    (cond
     ((not lore-web-mdn-enabled)
      (when (functionp done) (funcall done "mdn-disabled"))
      nil)
     ((or (null keywords) (null (car keywords)))
      (when (functionp done) (funcall done nil))
      nil)
     ((not (lore-web-confirm keywords 'mdn lore-web-mdn-confirm-privacy))
      (when (functionp done) (funcall done "privacy-declined"))
      nil)
     (t
      (let* ((url (lore-web-mdn--build-url lore-web-mdn-endpoint keywords)))
        (lore-web-mdn--http-fetch
         url
         (lambda (status body)
           (cond
            ((not (= status 200))
             (when (functionp done) (funcall done (format "http-%s" status))))
            (t
             (let* ((items (lore-web-mdn--parse-json body))
                    (res (lore-web-mdn--items->results (cl-subseq items 0 (min (length items) limit)))))
               (when (and (functionp emit) res) (funcall emit res))
               (when (functionp done) (funcall done nil)))))))
        (list :async t
              :token (format "web-mdn-%x" (random (ash 1 24)))
              :cancel (lambda () (message "MDN: cancel not supported for url.el fetch"))))))))

;; Optional registration
;;;###autoload
(with-eval-after-load 'lore-core
  (when (fboundp 'lore-register-getter)
    (lore-register-getter
     'web-mdn
     :capabilities '(:domains (web mdn) :scope (global) :kinds (url doc) :match (keyword))
     :fn #'lore-getter-web-mdn-run
     :cost 1.2
     :batch-p t)))

(provide 'lore-getter-web-mdn)
;;; lore-getter-web-mdn.el ends here
