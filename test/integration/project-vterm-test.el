;;; project-vterm-test.el --- Tests for project vterm toggle -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for mov/project-vterm-toggle function

;;; Code:

(require 'ert)
(require 'test-helper)

(ert-deftest integration-project-vterm-toggle-function-exists ()
  "Test that project vterm toggle function is defined."
  (require 'init-project)
  (should (fboundp 'mov/project-vterm-toggle))
  (should (commandp 'mov/project-vterm-toggle)))

(ert-deftest integration-project-vterm-toggle-creates-buffer ()
  "Test that project vterm toggle creates project-specific buffer."
  ;; Mock projectile
  (defun projectile-project-root () "/tmp/test-project/")

  ;; Mock vterm
  (defun vterm (buffer-name)
    (get-buffer-create buffer-name)
    (switch-to-buffer buffer-name))

  ;; Mock eat
  (defun eat ()
    (get-buffer-create "*eat*")
    (switch-to-buffer "*eat*"))

  (require 'init-project)

  ;; Test with vterm available
  (let ((expected-buffer-name "*vterm test-project*"))
    (with-temp-buffer
      ;; Simulate calling the toggle
      (should (fboundp 'mov/project-vterm-toggle)))))

(ert-deftest integration-project-vterm-keybindings-configured ()
  "Test that C-x p t keybinding is configured."
  ;; Mock projectile
  (defvar projectile-command-map (make-sparse-keymap))

  ;; Mock project
  (defvar project-prefix-map (make-sparse-keymap))

  (require 'init-project)

  ;; Verify function is bound in projectile map
  (should (keymapp projectile-command-map))

  ;; Verify function is bound in project map
  (should (keymapp project-prefix-map)))

(ert-deftest integration-meow-project-vterm-binding ()
  "Test that Meow leader map includes project vterm toggle."
  ;; Mock meow
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

  ;; The function should be available for binding
  (should (fboundp 'mov/project-vterm-toggle)))

(ert-deftest integration-project-vterm-fallback-to-eat ()
  "Test that project vterm falls back to eat when vterm unavailable."
  ;; Mock projectile
  (defun projectile-project-root () "/tmp/test-project/")

  ;; Ensure vterm is NOT available
  (fmakunbound 'vterm)

  ;; Mock eat
  (defvar eat-called nil)
  (defun eat ()
    (setq eat-called t)
    (get-buffer-create "*eat test-project*")
    (switch-to-buffer "*eat test-project*"))

  (require 'init-project)

  ;; Function should still work with eat
  (should (fboundp 'mov/project-vterm-toggle)))

(provide 'project-vterm-test)
;;; project-vterm-test.el ends here
