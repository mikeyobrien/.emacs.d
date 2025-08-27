;;; init-ai.el --- AI and productivity tools -*- lexical-binding: t -*-

;;; Commentary:
;; GPTel, MCP, and AI-related configuration

;;; Code:

;; Load secrets (contains API keys)
(load (expand-file-name "secrets.el" user-emacs-directory) t)

;; GPTel (AI assistant)
(define-prefix-command 'ai-map)
(global-set-key (kbd "C-c l") 'ai-map)

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu) 
  :config
  (setq claude-code-ide-terminal-backend 'eat)
  (claude-code-ide-emacs-tools-setup)) 


(use-package gptel
  :bind
  (("C-c l l" . gptel)
   ("C-c l r" . gptel-rewrite)
   ("C-c l m" . gptel-menu))
  :config
  (require 'gptel-integrations)

  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")
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
                gpt-oss:20b
                deepseek/deepseek-chat-v3.1)))
  
  ;; Define AWS Bedrock backend
  (defvar gptel-bedrock
    (gptel-make-bedrock "AWS"
      :stream t
      :region "us-west-2"
      :models '(claude-sonnet-4-20250514)
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
          gptel-model 'claude-sonnet-4-20250514)
    (message "Switched to AWS Bedrock backend"))
  
  (defun gptel-switch-backend ()
    "Toggle between Open WebUI and AWS Bedrock backends."
    (interactive)
    (if (eq gptel-backend gptel-openwebui)
        (gptel-switch-to-bedrock)
      (gptel-switch-to-openwebui)))
  
  ;; Set default backend to Open WebUI
  (setq gptel-backend gptel-openwebui
        gptel-model 'gpt-5-chat-latest
        gptel-default-mode 'org-mode)
  (add-hook 'gptel-mode-hook
            (lambda ()
              (setenv "AWS_PROFILE" "cline")))
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

;; MCP (Model Context Protocol)
(use-package mcp
  :ensure t
  :after gptel
  :custom (mcp-hub-servers
           `(("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" "/Users/mobrienv/workplace/")))
             ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))))	
  :config
  (require 'mcp-hub)
  :hook (after-init . mcp-hub-start-all-server))

;; GPTel commit messages
(use-package gptel-commit
  :after gptel
  :bind (:map magit-status-mode-map
              ("C-c c" . gptel-commit-generate)))

;; Backend switching key bindings
(define-key ai-map (kbd "s") 'gptel-switch-backend)
(define-key ai-map (kbd "o") 'gptel-switch-to-openwebui)
(define-key ai-map (kbd "b") 'gptel-switch-to-bedrock)

;; Q CLI key bindings
(global-set-key (kbd "C-c l q") 'open-q-cli-buffer)
(global-set-key (kbd "C-c l t") 'toggle-q-cli-buffer)
(global-set-key (kbd "C-c l r") 'gptel-q)

(provide 'init-ai)
;;; init-ai.el ends here
