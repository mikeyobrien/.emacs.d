;;; backends.el --- AI backend configurations -*- lexical-binding: t -*-

;;; Commentary:
;; OpenWebUI and AWS Bedrock backend setup

;;; Code:

(require 'gptel)

;; Define variables for API keys
(defvar openwebui-api-key nil
  "API key for OpenWebUI backend.")

;; Load secrets (contains API keys)
(let ((secrets-file (expand-file-name "secrets.el" user-emacs-directory)))
  (if (file-exists-p secrets-file)
      (load secrets-file)
    (warn "secrets.el not found - AI backends may not work properly.
Create secrets.el with your API keys (see secrets.el.example if available)")))

;; Define Open WebUI backend
(defvar gptel-openwebui
  (gptel-make-openai "OpenWebUI"
    :host "chat.mobrienv.dev"
    :key openwebui-api-key
    :endpoint "/api/chat/completions"
    :stream t
    :models '(gpt-5-chat-latest
              google/gemini-2.5-flash
              google/gemini-2.0-flash
              gpt-oss:120b
              gpt-oss:20b
              deepseek/deepseek-chat-v3.1)))

;; Define AWS Bedrock backend
(defvar gptel-bedrock
  (gptel-make-bedrock "AWS"
    :stream t
    :region "us-west-2"
    :models '(anthropic.claude-sonnet-4-5-20250929)
    :model-region 'us))

;; Backend switching functions
(defun gptel-switch-to-openwebui ()
  "Switch to Open WebUI backend."
  (interactive)
  (setq gptel-backend gptel-openwebui
        gptel-model 'gpt-5-chat-latest)
  (message "Switched to Open WebUI backend"))

(defun gptel-switch-to-bedrock ()
  "Switch to AWS Bedrock backend."
  (interactive)
  (setq gptel-backend gptel-bedrock
        gptel-model 'anthropic.claude-sonnet-4-5-20250929)
  (message "Switched to AWS Bedrock backend"))


(defun gptel-switch-backend ()
  "Toggle between Open WebUI and AWS Bedrock backends."
  (interactive)
  (if (eq gptel-backend gptel-openwebui)
      (gptel-switch-to-bedrock)
    (gptel-switch-to-openwebui)))

;; Set default backend
(setq gptel-backend gptel-openwebui
      gptel-model 'gpt-5-chat-latest)

(provide 'backends)
;;; backends.el ends here
