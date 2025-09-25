;;; lore-getter-info-quoted-test.el --- Tests for quoted info apropos lines -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-getter-info)

(ert-deftest lore-info-parse-quoted-eglot ()
  "Quoted output: \"(eglot)Starting Eglot\" -- activating Eglot for a project"
  (let ((line "\"(eglot)Starting Eglot\" -- activating Eglot for a project"))
    (should (equal (lore-getter-info--parse-line line)
                   '("eglot" "Starting Eglot" "activating Eglot for a project")))))

(ert-deftest lore-info-parse-classic-paren-colon ()
  "Classic output: (emacs) Buffers: Node about buffers..."
  (let ((line "(emacs) Buffers: Node about buffers..."))
    (should (equal (lore-getter-info--parse-line line)
                   '("emacs" "Buffers" "Node about buffers...")))))

(ert-deftest lore-info-parse-classic-noparen-colon ()
  "Alternate output: manual: Node: desc"
  (let ((line "libc: printf: Formatted output..."))
    (should (equal (lore-getter-info--parse-line line)
                   '("libc" "printf" "Formatted output...")))))

(ert-deftest lore-info-parse-garbage-nil ()
  "Garbage lines should return nil."
  (dolist (line '("" "random text" "\"(broken)"))
    (should (null (lore-getter-info--parse-line line)))))

(provide 'lore-getter-info-quoted-test)
;;; lore-getter-info-quoted-test.el ends here
