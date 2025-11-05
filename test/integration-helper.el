;;; integration-helper.el --- Integration test helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Utilities and mocks for integration testing

;;; Code:

(require 'ert)
(require 'test-helper)

;;; Mock Package System

(defvar integration-test-packages-loaded '()
  "List of packages loaded during integration tests.")

(defun integration-test-reset-state ()
  "Reset integration test state."
  (setq integration-test-packages-loaded '()))

(defmacro with-mocked-packages (packages &rest body)
  "Execute BODY with PACKAGES mocked.
PACKAGES is a list of package names as symbols."
  (declare (indent 1))
  `(let ((integration-test-packages-loaded '()))
     ;; Mock use-package
     (cl-letf (((symbol-function 'use-package)
                (lambda (name &rest args)
                  (push name integration-test-packages-loaded)
                  nil)))
       ,@body)))

;;; Mock Keybinding Functions

(defmacro with-mocked-keybindings (&rest body)
  "Execute BODY with keybinding functions mocked."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'global-set-key) (lambda (&rest _) nil))
             ((symbol-function 'define-key) (lambda (&rest _) nil))
             ((symbol-function 'local-set-key) (lambda (&rest _) nil)))
     ,@body))

;;; Mock Completion System

(defun integration-test-mock-completion-system ()
  "Mock the completion system (Vertico, Corfu, etc.)."
  ;; Mock Vertico
  (defvar vertico-mode nil)
  (defun vertico-mode (&optional arg)
    (setq vertico-mode (if arg (> arg 0) (not vertico-mode))))

  ;; Mock Marginalia
  (defvar marginalia-mode nil)
  (defun marginalia-mode (&optional arg)
    (setq marginalia-mode (if arg (> arg 0) (not marginalia-mode))))

  ;; Mock Corfu
  (defvar global-corfu-mode nil)
  (defun global-corfu-mode (&optional arg)
    (setq global-corfu-mode (if arg (> arg 0) (not global-corfu-mode))))

  ;; Mock Consult functions
  (defun consult-buffer () (interactive))
  (defun consult-line () (interactive))
  (defun consult-ripgrep () (interactive))
  (defun consult-find () (interactive))

  ;; Mock Embark
  (defun embark-act () (interactive))
  (defun embark-dwim () (interactive))
  (defun embark-act-all () (interactive)))

;;; Mock Modal Editing (Meow)

(defun integration-test-mock-meow ()
  "Mock Meow modal editing system."
  (defvar meow-mode nil)
  (defvar meow-mode-state-list '())
  (defvar meow-cheatsheet-layout-qwerty nil)
  (defvar meow-cursor-type-insert '(bar . 2))
  (defvar meow-cursor-type-normal 'box)

  (defun meow-mode (&optional arg)
    (setq meow-mode (if arg (> arg 0) (not meow-mode))))

  (defun meow-global-mode (&optional arg)
    (setq meow-mode (if arg (> arg 0) (not meow-mode))))

  (defun meow-setup-indicator ())

  (defmacro meow-motion-define-key (&rest _keys) nil)
  (defmacro meow-leader-define-key (&rest _keys) nil)
  (defmacro meow-normal-define-key (&rest _keys) nil))

;;; Mock Hydra

(defun integration-test-mock-hydra ()
  "Mock Hydra transient menu system."
  (defvar hydra-curr-map nil)

  (defmacro defhydra (name body &rest heads)
    "Mock defhydra macro."
    (let ((func-name (intern (format "%s/body" name))))
      `(defun ,func-name ()
         (interactive)
         (message "Mock hydra: %s" ',name)))))

;;; Mock AI/GPTel

(defun integration-test-mock-gptel ()
  "Mock GPTel AI integration."
  (defvar gptel-backend nil)
  (defvar gptel-model nil)

  (defun gptel-make-openai (name &rest args)
    "Mock gptel-make-openai."
    (list 'openai-backend :name name :args args))

  (defun gptel-make-bedrock (name &rest args)
    "Mock gptel-make-bedrock."
    (list 'bedrock-backend :name name :args args))

  (defun gptel-request (prompt &rest args)
    "Mock gptel-request."
    (message "Mock GPTel request: %s" prompt)
    nil)

  (defun gptel-rewrite ()
    "Mock gptel-rewrite."
    (interactive)
    (message "Mock GPTel rewrite")))

;;; Test Environment Setup

(defun integration-test-setup-environment ()
  "Set up a complete mock environment for integration tests."
  (integration-test-mock-completion-system)
  (integration-test-mock-meow)
  (integration-test-mock-hydra)
  (integration-test-mock-gptel))

;;; Test Assertions

(defun should-be-loadable (module)
  "Assert that MODULE can be loaded without errors."
  (should (require module nil t))
  (message "✓ Module %s loaded successfully" module))

(defun should-define-function (function-name)
  "Assert that FUNCTION-NAME is defined."
  (should (fboundp function-name))
  (message "✓ Function %s is defined" function-name))

(defun should-define-variable (variable-name)
  "Assert that VARIABLE-NAME is defined."
  (should (boundp variable-name))
  (message "✓ Variable %s is defined" variable-name))

(defun should-be-interactive (command)
  "Assert that COMMAND is an interactive command."
  (should (commandp command))
  (message "✓ Command %s is interactive" command))

;;; Integration Test Runner

(defmacro with-integration-test-environment (&rest body)
  "Run BODY in a complete integration test environment."
  (declare (indent 0))
  `(progn
     (integration-test-setup-environment)
     ,@body
     (integration-test-reset-state)))

(provide 'integration-helper)
;;; integration-helper.el ends here
