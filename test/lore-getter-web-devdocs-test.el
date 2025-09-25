;;; lore-getter-web-devdocs-test.el --- Tests for DevDocs web getter -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-getter-web-devdocs)
(require 'lore-web)
(require 'lore-model)

(ert-deftest lore-web-devdocs/parse-json-basic ()
  (let* ((json "{\"results\":[
{\"title\":\"Array.prototype.map\",\"url\":\"https://devdocs.io/javascript/global_objects/array/map\",\"summary\":\"map desc\"},
{\"title\":\"Object.entries\",\"url\":\"/javascript/global_objects/object/entries\",\"summary\":\"entries\"}
]}")
         (items (lore-web-devdocs--parse-json json)))
    (should (listp items))
    (should (= (length items) 2))
    (let ((r (car (lore-web-devdocs--items->results items))))
      (should (eq (lore-result-type r) 'url))
      (should (string-match-p "Array.prototype.map" (lore-result-title r)))
      (should (string-prefix-p "http" (lore-result-url r))))))

(ert-deftest lore-web-devdocs/disabled-early-exit ()
  (let* ((lore-web-devdocs-enabled nil)
         (done nil))
    (let ((ret (lore-getter-web-devdocs-run
                :request '((:keywords . ("map"))) :topk 5
                :emit (lambda (_b) (setq done 'emit))
                :done (lambda (&optional _e) (setq done t)))))
      (should (null ret))
      (should (eq done t)))))

(ert-deftest lore-web-devdocs/invalid-json ()
  (let* ((items (lore-web-devdocs--parse-json "{invalid json")))
    (should (listp items))
    (should (= (length items) 0))))

(ert-deftest lore-web-devdocs/http-error-calls-done ()
  (let ((lore-web-devdocs-enabled t)
        (lore-web-devdocs-confirm-privacy nil)
        (emitted nil) (done nil) (err nil))
    (cl-letf (((symbol-function 'lore-web-devdocs--http-fetch)
               (lambda (_url cb) (funcall cb 500 ""))))
      (let ((ret (lore-getter-web-devdocs-run
                  :request '((:keywords . ("map"))) :topk 3
                  :emit (lambda (_b) (setq emitted t))
                  :done (lambda (&optional e) (setq done t err e)))))
        (should (and (listp ret) (plist-get ret :async)))
        (should done)
        (should (string-prefix-p "http-" (or err ""))))
      (should (not emitted)))))

(ert-deftest lore-web-devdocs/privacy-declined ()
  (let ((lore-web-devdocs-enabled t)
        (lore-web-devdocs-confirm-privacy t)
        (lore-web-confirm-function (lambda (_msg) nil))
        (emitted nil) (done nil) (err nil))
    (let ((ret (lore-getter-web-devdocs-run
                :request '((:keywords . ("map"))) :topk 2
                :emit (lambda (_b) (setq emitted t))
                :done (lambda (&optional e) (setq done t err e)))))
      (should (null ret))
      (should done)
      (should (equal err "privacy-declined"))
      (should (not emitted)))))

(provide 'lore-getter-web-devdocs-test)
;;; lore-getter-web-devdocs-test.el ends here
