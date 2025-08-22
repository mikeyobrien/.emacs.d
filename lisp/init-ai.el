;;; init-ai.el --- AI and productivity tools -*- lexical-binding: t -*-

;;; Commentary:
;; GPTel, MCP, and AI-related configuration

;;; Code:

;; GPTel (AI assistant)
(define-prefix-command 'ai-map)
(global-set-key (kbd "C-c l") 'ai-map)
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

(defun open-q-cli-buffer (&optional project-path)
  "Create a dedicated Q CLI buffer that opens in EAT in a window at the bottom of frame that takes up 30%.
If PROJECT-PATH is provided, start Q CLI in that directory."
  (interactive)
  (let* ((default-directory (or project-path default-directory))
         (project-name (file-name-nondirectory (directory-file-name default-directory)))
         (buffer-name (format "*Q-CLI-%s*" project-name))
         (window-height (floor (* (frame-height) 0.3)))
         (q-buffer (get-buffer-create buffer-name)))
    ;; Split window at bottom and switch to it
    (select-window (split-window-below (- (window-height) window-height)))
    ;; Switch to the Q CLI buffer
    (switch-to-buffer q-buffer)
    ;; Start EAT in the buffer if not already running
    (unless (get-buffer-process q-buffer)
      (eat-mode)
      (eat-exec q-buffer "q" "q" nil '("chat" "-a")))
    ;; Make sure the window height is correct
    (window-resize (selected-window) 
                   (- window-height (window-height)) 
                   nil)))

(defun open-q-cli-project-buffer ()
  "Open Q CLI buffer for the current project root."
  (interactive)
  (let ((project-root (or (projectile-project-root)
                          default-directory)))
    (open-q-cli-buffer project-root)))

(defun refresh-q-cli-buffer ()
  "Refresh the Q CLI buffer by killing the current process and starting a new one."
  (interactive)
  (let* ((project-root (or (projectile-project-root) default-directory))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (buffer-name (format "*Q-CLI-%s*" project-name))
         (q-buffer (get-buffer buffer-name)))
    (when q-buffer
      (with-current-buffer q-buffer
        (when (get-buffer-process q-buffer)
          (kill-process (get-buffer-process q-buffer)))
        (let ((default-directory project-root))
          (eat-mode)
          (eat-exec q-buffer "q" "q" nil '("chat" "-a")))))))

(defun toggle-q-cli-buffer ()
  "Toggle the Q CLI buffer window for the current project."
  (interactive)
  (let* ((project-root (or (projectile-project-root) default-directory))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (buffer-name (format "*Q-CLI-%s*" project-name))
         (q-window (get-buffer-window buffer-name)))
    (if q-window
        (delete-window q-window)
      (open-q-cli-project-buffer))))

;; Q CLI key bindings
(global-set-key (kbd "C-c l q") 'open-q-cli-buffer)
(global-set-key (kbd "C-c l t") 'toggle-q-cli-buffer)
(global-set-key (kbd "C-c l r") 'refresh-q-cli-buffer)

(provide 'init-ai)
;;; init-ai.el ends here
