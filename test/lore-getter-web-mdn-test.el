;;; lore-getter-web-mdn-test.el --- Tests for MDN web getter -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-getter-web-mdn)
(require 'lore-web)
(require 'lore-model)

(ert-deftest lore-web-mdn/parse-json-basic ()
  (let* ((json "{\"documents\":[
{\"title\":\"Array.prototype.map()\",\"mdn_url\":\"/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map\",\"summary\":\"MDN map\"},
{\"title\":\"Promise\",\"mdn_url\":\"https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise\",\"summary\":\"Promise\"}
], \"base_url\":\"https://developer.mozilla.org\"}")
         (items (lore-web-mdn--parse-json json)))
    (should (listp items))
    (should (= (length items) 2))
    (let* ((res (lore-web-mdn--items->results items))
           (r1 (car res)))
      (should (eq (lore-result-type r1) 'url))
      (should (string-prefix-p "https://developer.mozilla.org" (lore-result-url r1))))))

(ert-deftest lore-web-mdn/disabled-early-exit ()
  (let* ((lore-web-mdn-enabled nil)
         (done nil))
    (let ((ret (lore-getter-web-mdn-run
                :request '((:keywords . ("map"))) :topk 5
                :emit (lambda (_b) (setq done 'emit))
                :done (lambda (&optional _e) (setq done t)))))
      (should (null ret))
      (should (eq done t)))))

(ert-deftest lore-web-mdn/invalid-json ()
  (let* ((items (lore-web-mdn--parse-json "{invalid json")))
    (should (listp items))
    (should (= (length items) 0))))

(ert-deftest lore-web-mdn/http-error-calls-done ()
  (let ((lore-web-mdn-enabled t)
        (lore-web-mdn-confirm-privacy nil)
        (emitted nil) (done nil) (err nil))
    (cl-letf (((symbol-function 'lore-web-mdn--http-fetch)
               (lambda (_url cb) (funcall cb 500 ""))))
      (let ((ret (lore-getter-web-mdn-run
                  :request '((:keywords . ("map"))) :topk 3
                  :emit (lambda (_b) (setq emitted t))
                  :done (lambda (&optional e) (setq done t err e)))))
        (should (and (listp ret) (plist-get ret :async)))
        (should done)
        (should (string-prefix-p "http-" (or err ""))))
      (should (not emitted)))))

(ert-deftest lore-web-mdn/privacy-declined ()
  (let ((lore-web-mdn-enabled t)
        (lore-web-mdn-confirm-privacy t)
        (lore-web-confirm-function (lambda (_msg) nil))
        (emitted nil) (done nil) (err nil))
    (let ((ret (lore-getter-web-mdn-run
                :request '((:keywords . ("map"))) :topk 2
                :emit (lambda (_b) (setq emitted t))
                :done (lambda (&optional e) (setq done t err e)))))
      (should (null ret))
      (should done)
      (should (equal err "privacy-declined"))
      (should (not emitted)))))

(provide 'lore-getter-web-mdn-test)
;;; lore-getter-web-mdn-test.el ends here
