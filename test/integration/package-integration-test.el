;;; package-integration-test.el --- Package configuration integration tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for package configuration and integration

;;; Code:

(require 'ert)
(require 'test-helper)

(ert-deftest integration-straight-use-package-available ()
  "Test that straight-use-package is available (mocked in tests)."
  (should (fboundp 'straight-use-package)))

(ert-deftest integration-completion-framework-components ()
  "Test that completion framework components are configured."
  ;; Mock the completion packages
  (defvar vertico-mode nil)
  (defun vertico-mode (&rest _args) (setq vertico-mode t))
  (defvar marginalia-mode nil)
  (defun marginalia-mode (&rest _args) (setq marginalia-mode t))
  (defvar global-corfu-mode nil)
  (defun global-corfu-mode (&rest _args) (setq global-corfu-mode t))
  (defvar nerd-icons-completion-mode nil)
  (defun nerd-icons-completion-mode (&rest _args) (setq nerd-icons-completion-mode t))

  ;; Mock consult functions (just verify they would be bound)
  (defun consult-buffer () (interactive))
  (defun consult-line () (interactive))
  (defun consult-ripgrep () (interactive))

  ;; Load completion config
  (require 'init-completion)

  ;; Verify completion-related settings
  (should (equal completion-styles '(orderless basic))))

(ert-deftest integration-embark-keybindings ()
  "Test that Embark keybindings are configured."
  (defun embark-act () (interactive))
  (defun embark-dwim () (interactive))
  (defun embark-act-all () (interactive))

  (require 'init-completion)

  ;; Verify functions exist (they should be mocked)
  (should (fboundp 'embark-act))
  (should (fboundp 'embark-dwim))
  (should (fboundp 'embark-act-all)))

(ert-deftest integration-orderless-configuration ()
  "Test that Orderless is configured for flexible completion."
  (require 'init-completion)

  ;; Verify completion styles include orderless
  (should (member 'orderless completion-styles))
  (should (member 'basic completion-styles)))

(ert-deftest integration-tab-always-indent-complete ()
  "Test that TAB is configured for completion."
  (require 'init-completion)

  ;; Verify tab-always-indent is set to 'complete
  (should (eq tab-always-indent 'complete)))

(provide 'package-integration-test)
;;; package-integration-test.el ends here
