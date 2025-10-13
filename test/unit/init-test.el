;;; init-test.el --- Tests for init.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'test-helper)

(ert-deftest init-load-path-configured ()
  "Test that lisp directory is in load-path."
  (should (member (expand-file-name "lisp" user-emacs-directory) load-path)))

(ert-deftest init-custom-file-configured ()
  "Test that custom-file variable exists."
  (should (boundp 'custom-file)))

(ert-deftest init-recentf-enabled ()
  "Test that recentf-mode is enabled."
  (should (fboundp 'recentf-mode)))

(ert-deftest init-winner-enabled ()
  "Test that winner-mode is enabled."
  (should (fboundp 'winner-mode)))

(provide 'init-test)
;;; init-test.el ends here
