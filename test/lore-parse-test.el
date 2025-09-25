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

(ert-deftest lore-parse/domain-inline-flags ()
  "Inline flags inside a domain token should be parsed and applied.
Example: \"man?scope=global: socket\" → targets=(man), scope=global, keywords=(socket)."
  (let* ((req (lore-parse-query "man?scope=global: socket ?k=7"))
         (targets (alist-get :targets req))
         (kws (alist-get :keywords req)))
    (should (memq 'man targets))
    (should (equal kws '("socket")))
    (should (eq (alist-get :scope req) 'global))
    (should (equal (alist-get :max-k req) 7))))

(ert-deftest lore-parse/domain-inline-flags-info ()
  "Inline flags for info should also work: \"info?scope=global: info\"."
  (let* ((req (lore-parse-query "info?scope=global: info")))
    (should (memq 'info (alist-get :targets req)))
    (should (eq (alist-get :scope req) 'global))
    (should (equal (alist-get :keywords req) '("info")))))

(ert-deftest lore-parse/preserve-case-for-keywords ()
  "Keywords should preserve original case to avoid breaking case-sensitive backends."
  (let* ((req (lore-parse-query "info: Eglot")))
    (should (memq 'info (alist-get :targets req)))
    (should (equal (alist-get :keywords req) '("Eglot")))))
