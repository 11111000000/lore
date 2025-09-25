;;; lore-getter-web-devdocs.el --- DevDocs web getter for Lore -*- lexical-binding: t; -*-

;;; Commentary:
;; Asynchronous web getter (optional, disabled by default).
;; Uses HTTP to query DevDocs-like endpoint and parses JSON into Lore results.
;; By default, this file only provides parser helpers and a guarded run
;; function that early-exits unless explicitly enabled.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'json)

(require 'lore-model nil t)
(require 'lore-core  nil t)
(require 'lore-web)

(defgroup lore-getter-web-devdocs nil
  "Web (DevDocs) getter for Lore."
  :group 'lore)

(defcustom lore-web-devdocs-enabled nil
  "When non-nil, enable DevDocs web getter."
  :type 'boolean
  :group 'lore-getter-web-devdocs)

(defcustom lore-web-devdocs-confirm-privacy t
  "When non-nil, ask confirmation before sending query to the web."
  :type 'boolean
  :group 'lore-getter-web-devdocs)

(defcustom lore-web-devdocs-endpoint "https://example.invalid/devdocs/search"
  "Base endpoint for DevDocs-like search API.
The actual DevDocs site doesn't expose a stable public search JSON endpoint.
This is a placeholder to demonstrate the getter. Integrators can set their proxy."
  :type 'string
  :group 'lore-getter-web-devdocs)

(defcustom lore-web-devdocs-timeout 3.0
  "HTTP timeout in seconds for DevDocs queries."
  :type 'number
  :group 'lore-getter-web-devdocs)

(defun lore-web-devdocs--parse-json (json-string)
  "Parse JSON-STRING into a list of alists: ((:title ...) (:url ...) (:summary ...))."
  (let* ((obj (condition-case _err
                  (json-parse-string json-string :object-type 'alist :array-type 'list :null-object nil)
                (error nil)))
         (arr (and (listp obj) (alist-get 'results obj))))
    (cl-loop for it in (or arr '())
             for title = (or (alist-get 'title it) (alist-get :title it) "")
             for url   = (or (alist-get 'url it)   (alist-get :url it)   "")
             for sum   = (or (alist-get 'summary it) (alist-get :summary it) "")
             when (and (stringp title) (stringp url))
             collect (list :title title :url url :summary (or sum "")))))

(defun lore-web-devdocs--items->results (items)
  "Convert parsed ITEMS (alist list with :title :url :summary) to lore-result list."
  (let ((score 1.0))
    (cl-loop for it in items
             for title = (plist-get it :title)
             for url   = (plist-get it :url)
             for sum   = (plist-get it :summary)
             collect
             (lore-result-create
              :type 'url
              :title (or title url)
              :snippet (or sum url)
              :content nil
              :path nil
              :url url
              :buffer nil
              :beg nil :end nil
              :score (prog1 score (setq score (max 0.0 (- score 0.02))))
              :source 'devdocs
              :meta (list :title title :summary sum)))))

(defun lore-web-devdocs--build-url (endpoint keywords)
  "Build URL from ENDPOINT and KEYWORDS list."
  (let* ((q (mapconcat #'identity keywords " "))
         (encoded (url-hexify-string q)))
    (format "%s?q=%s&format=json" (string-trim-right endpoint "/") encoded)))

(defun lore-web-devdocs--http-fetch (url callback)
  "Fetch URL asynchronously and call CALLBACK with (status body-string)."
  (let ((buf nil) (done nil))
    (url-retrieve
     url
     (lambda (h)
       (ignore h)
       (setq buf (current-buffer))
       (let* ((status (plist-get url-http-response-status 'symbol)) ; not reliable, fallback below
              (http-status (or (and (boundp 'url-http-response-status) url-http-response-status) 200))
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
    (run-at-time lore-web-devdocs-timeout nil
                 (lambda ()
                   (unless done
                     (when (buffer-live-p buf) (kill-buffer buf))
                     (funcall callback 408 ""))))))

;;;###autoload
(cl-defun lore-getter-web-devdocs-run (&key request topk emit done)
  "Run DevDocs web getter with REQUEST, TOPK; stream via EMIT, finalize via DONE.
Guarded by `lore-web-devdocs-enabled' and `lore-web-devdocs-confirm-privacy'."
  (let* ((keywords (or (alist-get :keywords request)
                       (let ((q (alist-get :query request)))
                         (and q (split-string q "[ \t\n]+" t)))))
         (limit (or topk 20)))
    (cond
     ((not lore-web-devdocs-enabled)
      (when (functionp done) (funcall done "devdocs-disabled"))
      nil)
     ((or (null keywords) (null (car keywords)))
      (when (functionp done) (funcall done nil))
      nil)
     ((not (lore-web-confirm keywords 'devdocs lore-web-devdocs-confirm-privacy))
      (when (functionp done) (funcall done "privacy-declined"))
      nil)
     (t
      (let* ((url (lore-web-devdocs--build-url lore-web-devdocs-endpoint keywords)))
        (lore-web-devdocs--http-fetch
         url
         (lambda (status body)
           (cond
            ((not (= status 200))
             (when (functionp done) (funcall done (format "http-%s" status))))
            (t
             (let* ((items (lore-web-devdocs--parse-json body))
                    (res (lore-web-devdocs--items->results (cl-subseq items 0 (min (length items) limit)))))
               (when (and (functionp emit) res) (funcall emit res))
               (when (functionp done) (funcall done nil)))))))
        (list :async t
              :token (format "web-devdocs-%x" (random (ash 1 24)))
              :cancel (lambda () (message "DevDocs: cancel not supported for url.el fetch"))))))))

;; Optional registration
;;;###autoload
(with-eval-after-load 'lore-core
  (when (fboundp 'lore-register-getter)
    (lore-register-getter
     'web-devdocs
     :capabilities '(:domains (web devdocs) :scope (global) :kinds (url doc) :match (keyword))
     :fn #'lore-getter-web-devdocs-run
     :cost 1.2
     :batch-p t)))

(provide 'lore-getter-web-devdocs)
;;; lore-getter-web-devdocs.el ends here
