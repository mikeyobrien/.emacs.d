;;; ai-integration-test.el --- AI integration tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for AI backend and helper integration

;;; Code:

(require 'ert)
(require 'test-helper)

(ert-deftest integration-ai-backend-switching ()
  "Test that AI backend switching functions work."
  ;; Mock gptel
  (defvar gptel-backend nil)
  (defvar gptel-model nil)
  (defvar gptel-openwebui 'openwebui-backend)
  (defvar gptel-bedrock 'bedrock-backend)

  (defun gptel-make-openai (_name &rest _args) 'openwebui-backend)
  (defun gptel-make-bedrock (_name &rest _args) 'bedrock-backend)

  ;; Set up test key
  (setq openwebui-api-key "test-key")

  ;; Load backends
  (require 'backends)

  ;; Test switching to Bedrock
  (gptel-switch-to-bedrock)
  (should (eq gptel-backend gptel-bedrock))
  (should (eq gptel-model 'anthropic.claude-sonnet-4-5-20250929))

  ;; Test switching to OpenWebUI
  (gptel-switch-to-openwebui)
  (should (eq gptel-backend gptel-openwebui))
  (should (eq gptel-model 'gpt-5-chat-latest)))

(ert-deftest integration-ai-backend-toggle ()
  "Test that backend toggle switches between backends."
  (defvar gptel-backend nil)
  (defvar gptel-model nil)
  (defvar gptel-openwebui 'openwebui-backend)
  (defvar gptel-bedrock 'bedrock-backend)

  (defun gptel-make-openai (_name &rest _args) 'openwebui-backend)
  (defun gptel-make-bedrock (_name &rest _args) 'bedrock-backend)

  (setq openwebui-api-key "test-key")
  (require 'backends)

  ;; Start with OpenWebUI
  (setq gptel-backend gptel-openwebui)

  ;; Toggle should switch to Bedrock
  (gptel-switch-backend)
  (should (eq gptel-backend gptel-bedrock))

  ;; Toggle again should switch back
  (gptel-switch-backend)
  (should (eq gptel-backend gptel-openwebui)))

(ert-deftest integration-ai-helpers-region-or-buffer ()
  "Test AI helper function for getting region or buffer content."
  ;; Mock gptel-request
  (defun gptel-request (_prompt &rest _args) nil)

  (require 'helpers)

  ;; Test with buffer content
  (with-temp-buffer
    (insert "test content")
    (should (equal "test content" (mov/gptel--region-or-buffer-string))))

  ;; Test with region
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    (set-mark (point))
    (forward-line 1)
    (activate-mark)
    (should (string-match-p "line1" (mov/gptel--region-or-buffer-string)))))

(ert-deftest integration-ai-helpers-all-functions-interactive ()
  "Test that AI helper functions are interactive commands."
  (defun gptel-request (_prompt &rest _args) nil)
  (defun gptel-rewrite () (interactive))

  (require 'helpers)

  ;; All main helper functions should be interactive
  (should (commandp 'mov/gptel-explain-region))
  (should (commandp 'mov/gptel-docstring-region))
  (should (commandp 'mov/gptel-tests-region))
  (should (commandp 'mov/gptel-refactor-region))
  (should (commandp 'mov/gptel-complete))
  (should (commandp 'mov/gptel-insert)))

(ert-deftest integration-ai-model-id-consistency ()
  "Test that model IDs are consistent across backend definitions."
  (defvar gptel-backend nil)
  (defvar gptel-model nil)
  (defvar gptel-openwebui 'openwebui-backend)
  (defvar gptel-bedrock 'bedrock-backend)

  (defun gptel-make-openai (_name &rest args)
    (plist-get args :models))
  (defun gptel-make-bedrock (_name &rest args)
    (plist-get args :models))

  (setq openwebui-api-key "test-key")
  (require 'backends)

  ;; Switch to Bedrock and verify model matches
  (gptel-switch-to-bedrock)
  ;; The model set in gptel-model should match one from the backend
  (should (eq gptel-model 'anthropic.claude-sonnet-4-5-20250929)))

(provide 'ai-integration-test)
;;; ai-integration-test.el ends here
