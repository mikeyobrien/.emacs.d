;;; init-ai.el --- AI configuration loader -*- lexical-binding: t -*-

;;; Commentary:
;; GPTel, MCP, and AI-related configuration

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp/ai" user-emacs-directory))

;; Main gptel setup
(use-package gptel
  :demand t
  :config
  (require 'gptel-integrations)
  (setq gptel-use-curl t
        gptel-default-mode 'org-mode
        gptel-disabled-modes '(markdown-mode))
  
  ;; Org-mode prompt prefixes
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")
  
  ;; Auto-enable gptel-mode
  (defun mov/gptel-mode-auto ()
    "Ensure that this file opens with `gptel-mode' enabled."
    (save-excursion
      (let ((enable-local-variables t))
        (if (and (save-excursion
                   (goto-char (point-min))
                   (looking-at ".*-\\*-")))
            (modify-file-local-variable-prop-line 'eval nil 'delete))
        (add-file-local-variable-prop-line
         'eval '(and (fboundp 'gptel-mode) (gptel-mode 1))))))
  
  (add-hook 'gptel-save-state-hook #'mov/gptel-mode-auto)
  (add-hook 'gptel-mode-hook
            (lambda ()
              (setenv "AWS_PROFILE" "cline")))
  
  ;; Display rules
  (add-to-list 'display-buffer-alist
               '("\\*gptel\\*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.33)))
  (add-to-list 'display-buffer-alist
               '("\\*gptel-lookup\\*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.25))))

;; Claude Code IDE
(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (setq claude-code-ide-terminal-backend 'eat)
  (claude-code-ide-emacs-tools-setup))

;; GPTel commit messages
(use-package gptel-commit
  :after gptel
  :bind ((:map magit-status-mode-map
               ("C-c c" . gptel-commit-generate))
         (:map git-commit-mode-map
               ("C-c C-a" . gptel-commit))))

;; gptel-quick
(require 'thingatpt)

(defvar gptel-quick-display (and (fboundp 'posframe-workable-p) 'posframe))
(defvar gptel-quick-system-message
  (lambda (count) (format "Explain in %d words or fewer." count)))
(defvar gptel-quick-word-count 12)
(defvar gptel-quick-timeout 10)
(defvar gptel-quick-use-context nil)
(defvar gptel-quick-backend nil)
(defvar gptel-quick-model nil)

(defvar gptel-lookup--history nil)

(defun gptel-lookup (prompt)
  "Quick AI lookup with PROMPT."
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
         (display-buffer (current-buffer)
                         `((display-buffer-in-side-window)
                           (side . bottom)
                           (window-height . ,#'fit-window-to-buffer))))))))

;; Load AI modules
(require 'backends)
(require 'helpers)
(require 'integrations)
(require 'mcp)
(require 'keybindings)

(provide 'init-ai)
;;; init-ai.el ends here
