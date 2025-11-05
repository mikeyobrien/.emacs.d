;;; keybinding-integration-test.el --- Keybinding integration tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for keybinding setup and integration

;;; Code:

(require 'ert)
(require 'test-helper)

(ert-deftest integration-meow-setup-defines-keybindings ()
  "Test that Meow setup defines expected keybindings."
  ;; Mock meow functions
  (defun meow-mode (&rest _args) t)
  (defun meow-global-mode (&rest _args) t)
  (defun meow-setup-indicator (&rest _args) t)
  (defvar meow-cheatsheet-layout-qwerty nil)
  (defvar meow-cursor-type-insert '(bar . 2))
  (defvar meow-cursor-type-normal 'box)
  (defvar meow-mode-state-list '())

  ;; Mock meow key definition functions
  (defmacro meow-motion-define-key (&rest _keys) nil)
  (defmacro meow-leader-define-key (&rest _keys) nil)
  (defmacro meow-normal-define-key (&rest _keys) nil)

  ;; Load meow config
  (require 'init-hydras)
  (require 'init-meow)

  ;; Verify meow-setup function exists
  (should (fboundp 'meow-setup)))

(ert-deftest integration-meow-terminal-integration ()
  "Test that terminal modes are configured for insert mode in Meow."
  (defvar meow-mode-state-list '())
  (defun meow-mode (&rest _args) t)
  (defun meow-global-mode (&rest _args) t)
  (defun meow-setup-indicator (&rest _args) t)
  (defvar meow-cheatsheet-layout-qwerty nil)
  (defvar meow-cursor-type-insert '(bar . 2))
  (defvar meow-cursor-type-normal 'box)

  (defmacro meow-motion-define-key (&rest _keys) nil)
  (defmacro meow-leader-define-key (&rest _keys) nil)
  (defmacro meow-normal-define-key (&rest _keys) nil)

  (require 'init-hydras)
  (require 'init-meow)

  ;; Terminal modes should be in insert mode
  (should (assq 'eat-mode meow-mode-state-list))
  (should (assq 'vterm-mode meow-mode-state-list))
  (should (assq 'term-mode meow-mode-state-list))
  (should (assq 'eshell-mode meow-mode-state-list)))

(ert-deftest integration-meow-leader-map-has-org-support ()
  "Test that Meow leader map includes org-mode commands."
  (defvar meow-mode-state-list '())
  (defun meow-mode (&rest _args) t)
  (defun meow-global-mode (&rest _args) t)
  (defun meow-setup-indicator (&rest _args) t)
  (defvar meow-cheatsheet-layout-qwerty nil)
  (defvar meow-cursor-type-insert '(bar . 2))
  (defvar meow-cursor-type-normal 'box)

  (defmacro meow-motion-define-key (&rest _keys) nil)
  (defmacro meow-leader-define-key (&rest _keys) nil)
  (defmacro meow-normal-define-key (&rest _keys) nil)

  (require 'init-hydras)
  (require 'init-meow)

  ;; Verify localleader map is defined
  (should (commandp 'mov/localleader-map)))

(ert-deftest integration-hydra-definitions-exist ()
  "Test that hydra definitions are available."
  (defvar hydra-curr-map nil)
  (defmacro defhydra (_name _body &rest _heads) nil)

  (require 'init-hydras)

  ;; Core hydras should be defined
  (should (fboundp 'hydra-scroll/body))
  (should (fboundp 'hydra-window/body))
  (should (fboundp 'hydra-zoom/body))
  (should (fboundp 'hydra-rectangle/body)))

(provide 'keybinding-integration-test)
;;; keybinding-integration-test.el ends here
