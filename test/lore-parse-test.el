;;; lore-parse-test.el --- Tests for lore-core parsing -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-core)

(ert-deftest lore-parse/basic-keywords ()
  (let* ((req (lore-parse-query "foo bar"))
         (kws (alist-get :keywords req)))
    (should (equal kws '("foo" "bar")))))

(ert-deftest lore-parse/prefix-targets-and-flags ()
  (let* ((req (lore-parse-query "elisp: map ?k=5 ?scope=global #util"))
         (targets (alist-get :targets req))
         (kws (alist-get :keywords req))
         (filters (alist-get :filters req)))
    (should (memq 'elisp targets))
    (should (equal kws '("map")))
    (should (equal (alist-get :max-k req) 5))
    (should (eq (alist-get :scope req) 'global))
    (should (equal (alist-get :tag filters) "util"))))
