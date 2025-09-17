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
   ("C-c l a" . gptel-lookup)
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

  (defvar gptel-lookup--history nil)

(defun gptel-lookup (prompt)
  (interactive (list (read-string "Ask AI: " nil gptel-lookup--history)))
  (when (string= prompt "") (user-error "A prompt is required."))
  (gptel-request
   prompt
   :callback
   (lambda (response info)
     (if (not response)
         (message "gptel-lookup failed with message: %s" (plist-get info :status))
       (with-current-buffer (get-buffer-create "*gptel-lookup*")
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert response))
	 (special-mode)
         (org-mode)
         (display-buffer (current-buffer)
                         `((display-buffer-in-side-window)
                           (side . bottom)
                           (window-height . ,#'fit-window-to-buffer))))))))

  (defun mov/gptel-mode-auto ()
  "Ensure that this file opens with `gptel-mode' enabled."
  (save-excursion
    (let ((enable-local-variables t))  ; Ensure we can modify local variables
      (if (and (save-excursion
                 (goto-char (point-min))
                 (looking-at ".*-\\*-")))  ; If there's a -*- line
        ;; First remove any existing eval, then add the new one
        (modify-file-local-variable-prop-line
          'eval nil 'delete))
      ;; Always add our eval
      (add-file-local-variable-prop-line
        'eval '(and (fboundp 'gptel-mode) (gptel-mode 1))))))

  (add-hook 'gptel-save-state-hook #'mov/gptel-mode-auto)
  
  ;; Set default backend to Open WebUI
  (setq gptel-backend gptel-openwebui
        gptel-model 'gpt-5-chat-latest
        gptel-default-mode 'org-mode)
  (add-hook 'gptel-mode-hook
            (lambda ()
              (setenv "AWS_PROFILE" "cline"))))

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
  :bind ((:map magit-status-mode-map
               ("C-c c" . gptel-commit-generate))
         (:map git-commit-mode-map
               ("C-c C-a" . gptel-commit))))

;; Backend switching key bindings
(define-key ai-map (kbd "s") 'gptel-switch-backend)
(define-key ai-map (kbd "o") 'gptel-switch-to-openwebui)
(define-key ai-map (kbd "b") 'gptel-switch-to-bedrock)

(provide 'init-ai)
;;; init-ai.el ends here
