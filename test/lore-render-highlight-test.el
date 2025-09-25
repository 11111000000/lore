;;; lore-render-highlight-test.el --- Tests for render keyword highlighting -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-render)
(require 'lore-model)

(ert-deftest lore-render/highlight-keywords ()
  (let* ((lore-render-highlight-keywords t)
         (kw '("foo"))
         (r (lore-result-create :type 'doc :title "Foo Title" :snippet "bar foo baz" :source 'elisp))
         (lines (lore-render-lines (list r) nil kw))
         (line (car lines)))
    (should (stringp line))
    (should (string-match "foo" line))
    (let ((pos (match-beginning 0)))
      (should (get-text-property pos 'face line)))))

(ert-deftest lore-render/highlight-disabled ()
  (let* ((lore-render-highlight-keywords nil)
         (kw '("foo"))
         (r (lore-result-create :type 'doc :title "Foo Title" :snippet "bar foo baz" :source 'elisp))
         (lines (lore-render-lines (list r) nil kw))
         (line (car lines)))
    (should (stringp line))
    (should (string-match "foo" line))
    (let ((pos (match-beginning 0)))
      (should-not (get-text-property pos 'face line)))))

(provide 'lore-render-highlight-test)
;;; lore-render-highlight-test.el ends here
