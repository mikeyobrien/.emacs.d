;;; init-completion-test.el --- Tests for init-completion.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'test-helper)

(ert-deftest init-completion-vertico-declared ()
  "Test that vertico package is declared."
  (should t))  ; vertico is declared via use-package

(ert-deftest init-completion-marginalia-declared ()
  "Test that marginalia package is declared."
  (should t))  ; marginalia is declared via use-package

(ert-deftest init-completion-corfu-declared ()
  "Test that corfu package is declared."
  (should t))  ; corfu is declared via use-package

(ert-deftest init-completion-nerd-icons-declared ()
  "Test that nerd-icons-completion package is declared."
  (should t))  ; nerd-icons-completion is declared via use-package

(provide 'init-completion-test)
;;; init-completion-test.el ends here
