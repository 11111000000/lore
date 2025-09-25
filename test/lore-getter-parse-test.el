;;; lore-getter-parse-test.el --- Unit tests for getter parsers -*- lexical-binding: t; -*-

(require 'ert)
(require 'lore-getter-grep)
(require 'lore-getter-info)
(require 'lore-getter-man)

(ert-deftest lore-grep/parse-windows-path ()
  "Ensure Windows-style paths are parsed correctly."
  (let* ((line "C:\\proj\\src\\main.c:12:3:hello world")
         (parsed (lore-getter-grep--parse-line line)))
    (should parsed)
    (pcase-let ((`(,path ,ln ,col ,txt) parsed))
      (should (string-prefix-p "C:\\" path))
      (should (= ln 12))
      (should (= col 3))
      (should (string= txt "hello world")))))

(ert-deftest lore-info/parse-alt-format ()
  "Info apropos alternative 'manual: Node: desc' format."
  (let* ((line "emacs: Buffers: Manage the list of buffers")
         (parsed (lore-getter-info--parse-line line)))
    (should parsed)
    (pcase-let ((`(,file ,node ,desc) parsed))
      (should (string= file "emacs"))
      (should (string= node "Buffers"))
      (should (string-match-p "Manage the list" desc)))))

(ert-deftest lore-man/parse-en-dash-and-posix-section ()
  "Man -k output may use en dash and posix section name."
  (let* ((line "printf (3posix) – formatted output")
         (parsed (lore-getter-man--parse-line line)))
    (should parsed)
    (pcase-let ((`(,name ,section ,desc) parsed))
      (should (string= name "printf"))
      (should (string= section "3posix"))
      (should (string-match-p "formatted output" desc)))))

(ert-deftest lore-info/parse-parens-format ()
  "GNU info apropos typical '(manual) Node: desc' format."
  (let* ((line "(emacs) Buffers: Manage the list of buffers")
         (parsed (lore-getter-info--parse-line line)))
    (should parsed)
    (pcase-let ((`(,file ,node ,desc) parsed))
      (should (string= file "emacs"))
      (should (string= node "Buffers"))
      (should (string-match-p "Manage the list" desc)))))

(ert-deftest lore-info/parse-negative ()
  "Lines that don't follow known patterns should return nil."
  (dolist (line '("nonsense line"
                  "emacs Buffers Manage the list"
                  "(manual node: missing parens"
                  "manual: node - wrong separator"))
    (should (null (lore-getter-info--parse-line line)))))

(ert-deftest lore-man/parse-negative ()
  "Man lines without expected '(section) - desc' should return nil."
  (dolist (line '("printf 3 formatted output"
                  "printf - formatted output"
                  "printf () - desc"
                  " - (3) desc"))
    (should (null (lore-getter-man--parse-line line)))))

(ert-deftest lore-grep/parse-negative ()
  "Ripgrep parse should fail gracefully on non-matching lines."
  (dolist (line '("just some text"
                  "/path/with:colon but no numbers"
                  "file.c:abc:def:missing numbers"
                  "file.c:12:hello missing col"
                  "C:\\path\\no-line-col:only:text"))
    (should (null (lore-getter-grep--parse-line line)))))

(provide 'lore-getter-parse-test)
;;; lore-getter-parse-test.el ends here
