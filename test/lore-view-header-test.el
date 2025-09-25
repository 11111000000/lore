;;; lore-view-header-test.el --- Tests for Lore header formatting -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-view)

(ert-deftest lore-view/header-format-basic ()
  (let* ((q "elisp: map")
         (n 3)
         (cached-p nil)
         (req '((:scope . project) (:targets . (elisp project)))))
    (let ((hdr (lore-view--format-header q n cached-p req)))
      (should (string-match-p "^🧭 Lore: elisp: map — 3 result" hdr))
      (should (string-match-p "\\[scope: project; domains: elisp, project\\]" hdr))
      (should-not (string-match-p "(cached)$" hdr)))))

(ert-deftest lore-view/header-format-cached-and-global ()
  (let* ((q "org: план #work ?scope=global")
         (n 1)
         (cached-p t)
         (req '((:scope . global) (:targets . (org)))))
    (let ((hdr (lore-view--format-header q n cached-p req)))
      (should (string-match-p "— 1 result\\>" hdr))
      (should (string-match-p "\\[scope: global; domains: org\\]" hdr))
      (should (string-match-p "(cached)$" hdr)))))

(ert-deftest lore-view/header-format-no-targets ()
  (let* ((q "just words")
         (n 0)
         (cached-p nil)
         (req '((:scope . project) (:targets . ()))))
    (let ((hdr (lore-view--format-header q n cached-p req)))
      (should (string-match-p "\\[scope: project\\]" hdr))
      ;; No "; domains:" segment if none specified
      (should-not (string-match-p "; domains:" hdr)))))

(provide 'lore-view-header-test)
;;; lore-view-header-test.el ends here
