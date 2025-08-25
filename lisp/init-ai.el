;;; init-ai.el --- AI and productivity tools -*- lexical-binding: t -*-

;;; Commentary:
;; GPTel, MCP, and AI-related configuration

;;; Code:

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
  :init
  (setq gptel-model 'claude-sonnet-4-20250514
        gptel-backend (gptel-make-bedrock "AWS"
                        :stream t
                        :region "us-west-2"
                        :models '(claude-sonnet-4-20250514)
                        :model-region 'us)
        gptel-default-mode 'markdown-mode)
  :bind
  (("C-c l l" . gptel)
   ("C-c l r" . gptel-rewrite)
   ("C-c l m" . gptel-menu))
  :config
  (require 'gptel-integrations)
  (add-hook 'buffer-list-update-hook
            (lambda ()
              (when (string= (buffer-name) "*AWS*")
                (markdown-mode))))
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

;; Q CLI key bindings
(global-set-key (kbd "C-c l q") 'open-q-cli-buffer)
(global-set-key (kbd "C-c l t") 'toggle-q-cli-buffer)
(global-set-key (kbd "C-c l r") 'refresh-q-cli-buffer)

(provide 'init-ai)
;;; init-ai.el ends here
