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

  ;; Prefer curl transport for robust streaming
  (setq gptel-use-curl t)

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
      :models '(claude-3-5-sonnet-latest
                claude-3-5-haiku-latest
                claude-sonnet-4-20250514)
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
         (org-mode)
         (display-buffer (current-buffer)
                         '((display-buffer-in-side-window)
                           (side . bottom)
                           (window-height . fit-window-to-buffer))))))))

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
        gptel-default-mode 'org-mode
        gptel-temperature 0.2
        gptel-max-tokens 8000)
  (add-hook 'gptel-mode-hook
            (lambda ()
              (setenv "AWS_PROFILE" "cline"))))

;; MCP (Model Context Protocol)
(use-package mcp
  :ensure t
  :after gptel
  :config
  (let* ((filesystem-root "/Users/mobrienv/workplace/")
         (servers (list (cons "fetch" '(:command "uvx" :args ("mcp-server-fetch"))))))
    (when (file-directory-p filesystem-root)
      (push (cons "filesystem"
                  `(:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" ,filesystem-root)))
            servers))
    (setq mcp-hub-servers servers))
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

;;
;; Display and window routing for GPTel buffers
;;
(with-eval-after-load 'gptel
  (add-to-list 'display-buffer-alist
               (cons "\\*gptel\\*"
                     '((display-buffer-in-side-window)
                       (side . bottom)
                       (window-height . 0.33))))
  (add-to-list 'display-buffer-alist
               (cons "\\*gptel-lookup\\*"
                     '((display-buffer-in-side-window)
                       (side . bottom)
                       (window-height . 0.25)))))

;;
;; Helper commands for common one-shot tasks
;;
(defun mov/gptel--region-or-buffer-string ()
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun mov/gptel-explain-region ()
  "Explain the selected code/text or current buffer."
  (interactive)
  (let ((content (mov/gptel--region-or-buffer-string)))
    (gptel-request
     (format "Explain the following content clearly and concisely.\n\n%s" content)
     :system "You are a senior engineer. Provide a precise, succinct explanation, include gotchas and edge cases when relevant."
     :buffer (get-buffer-create "*gptel-explain*"))))

(defun mov/gptel-docstring-region ()
  "Write docstrings/comments for the selected code or buffer."
  (interactive)
  (let ((content (mov/gptel--region-or-buffer-string)))
    (gptel-request
     (format "Write idiomatic docstrings/comments for this code without changing behavior.\n\n%s" content)
     :system "Return only the improved code or docstrings as appropriate."
     :buffer (get-buffer-create "*gptel-docstring*"))))

(defun mov/gptel-tests-region ()
  "Generate focused tests for the selected code or buffer."
  (interactive)
  (let ((content (mov/gptel--region-or-buffer-string)))
    (gptel-request
     (format "Write unit tests for this code. Include edge cases and name tests descriptively.\n\n%s" content)
     :system "Prefer existing project test frameworks and helpers. Return only test code."
     :buffer (get-buffer-create "*gptel-tests*"))))

(defun mov/gptel-refactor-region ()
  "Refactor the selected code or buffer without changing behavior."
  (interactive)
  (let ((content (mov/gptel--region-or-buffer-string)))
    (gptel-request
     (format "Refactor to improve readability without altering behavior.\n\n%s" content)
     :system "Keep changes minimal and idiomatic. Return only code."
     :buffer (get-buffer-create "*gptel-refactor*"))))

(define-key ai-map (kbd "E") #'mov/gptel-explain-region)
(define-key ai-map (kbd "D") #'mov/gptel-docstring-region)
(define-key ai-map (kbd "T") #'mov/gptel-tests-region)
(define-key ai-map (kbd "R") #'mov/gptel-refactor-region)

;;
;; Dired and EWW integrations
;;
(with-eval-after-load 'dired
  (defun mov/gptel-add-files-from-dired ()
    "Attach marked files in Dired to the current gptel chat."
    (interactive)
    (let ((files (dired-get-marked-files)))
      (dolist (f files)
        (gptel-add-file f))
      (message "Attached %d file(s) to GPTel" (length files))))
  (define-key ai-map (kbd "A") #'mov/gptel-add-files-from-dired))

(with-eval-after-load 'eww
  (defun mov/gptel-eww-summarize ()
    "Summarize the current EWW page in a side buffer."
    (interactive)
    (let* ((buf (current-buffer))
           (title (or (plist-get eww-data :title) (buffer-name)))
           (url (or (plist-get eww-data :url) ""))
           (text (with-current-buffer buf
                   (buffer-substring-no-properties (point-min) (point-max)))))
      (gptel-request
       (format "Summarize this page for later reference. Include key points and actions.\n\nTitle: %s\nURL: %s\n\n%s" title url text)
       :buffer (get-buffer-create "*gptel-eww*"))))
  (define-key ai-map (kbd "S") #'mov/gptel-eww-summarize))

;;
;; Model and temperature toggles
;;
(defun mov/gptel-toggle-fast-quality ()
  "Toggle between fast and high-quality models for current backend."
  (interactive)
  (cond
   ((eq gptel-backend gptel-openwebui)
    (setq gptel-model (if (eq gptel-model 'gpt-5-chat-latest)
                          'google/gemini-2.5-flash
                        'gpt-5-chat-latest))
    (message "OpenWebUI model: %s" gptel-model))
   ((eq gptel-backend gptel-bedrock)
    (setq gptel-model (if (memq gptel-model '(claude-sonnet-4-20250514))
                          'claude-3-5-sonnet-latest
                        'claude-sonnet-4-20250514))
    (message "Bedrock model: %s" gptel-model))
   (t (message "Unknown backend; no toggle applied"))))

(defun mov/gptel-bump-temperature ()
  "Toggle temperature between focused and creative."
  (interactive)
  (setq gptel-temperature (if (< (or gptel-temperature 0.2) 0.5) 0.7 0.2))
  (message "gptel-temperature: %.1f" gptel-temperature))

(define-key ai-map (kbd "q") #'mov/gptel-toggle-fast-quality)
(define-key ai-map (kbd "t") #'mov/gptel-bump-temperature)

;; Removed: post-hoc mutation of Bedrock backend (caused listp errors).

;;
;; Simple pre-send guard for expensive settings
;;
(defun mov/gptel--expensive-model-p (model)
  (memq model '(gpt-5-chat-latest claude-sonnet-4-20250514 claude-3-5-sonnet-latest)))

(defun mov/gptel-send-guard (orig-fun &rest args)
  "Warn when using expensive settings or very large inputs before sending."
  (let* ((model (and (boundp 'gptel-model) gptel-model))
         (chars (buffer-size))
         (too-long (> chars 200000))
         (expensive (mov/gptel--expensive-model-p model))
         (huge-max (and (boundp 'gptel-max-tokens) (> (or gptel-max-tokens 0) 12000))))
    (if (and (or expensive huge-max too-long)
             (not (yes-or-no-p (format "Send with model=%s, tokens=%s, size=%s chars? "
                                       model gptel-max-tokens chars))))
        (message "Send canceled by user")
      (apply orig-fun args))))

(with-eval-after-load 'gptel
  (when (fboundp 'gptel-send)
    (advice-add 'gptel-send :around #'mov/gptel-send-guard)))

;;
;; Per-project system prompt loader (AGENTS.md, etc.)
;;
(require 'project)
(require 'seq)
(defun mov/gptel--read-file-first-k (file k)
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-substring-no-properties (point-min) (min (point-max) (+ (point-min) k))))))

(defun mov/gptel--project-root ()
  (when-let* ((pr (project-current nil)))
    (car (project-roots pr))))

(defun mov/gptel--project-system-prompt ()
  "Return content for a project system prompt, if any."
  (let* ((root (mov/gptel--project-root))
         (candidates (when root
                       (seq-filter #'identity
                                   (mapcar (lambda (f) (expand-file-name f root))
                                           '("AGENTS.md" "GUIDELINES.md" "CONTRIBUTING.md" ".gptel/system-prompt.org" ".gptel/system-prompt.txt")))))
         (existing (seq-find #'file-exists-p candidates)))
    (mov/gptel--read-file-first-k existing 8000)))

(defun mov/gptel-apply-project-system-prompt ()
  "Apply project system prompt to current gptel buffer, if available."
  (interactive)
  (when (derived-mode-p 'gptel-mode)
    (when-let* ((prompt (mov/gptel--project-system-prompt)))
      (setq-local gptel-system-prompt prompt)
      (message "Applied project system prompt from repo."))))

(add-hook 'gptel-mode-hook #'mov/gptel-apply-project-system-prompt)

;; Optional helper: write a sample .dir-locals.el into current project
(defun mov/gptel-write-dir-locals-example ()
  "Create a sample .dir-locals.el for gptel in the current project."
  (interactive)
  (let ((root (mov/gptel--project-root)))
    (unless root (user-error "No project detected"))
    (let* ((file (expand-file-name ".dir-locals.el" root))
           (content "((nil . ((gptel-model . gpt-5-chat-latest)
                          (gptel-temperature . 0.2)
                          (gptel-system-prompt . \"Project rules and context here...\")))))\n"))
      (if (file-exists-p file)
          (user-error ".dir-locals.el already exists at %s" root)
        (with-temp-file file (insert content))
        (message "Wrote example %s" file)))))

(define-key ai-map (kbd "L") #'mov/gptel-write-dir-locals-example)

;;
;; Transient menu for AI actions under C-c l .
;;
(require 'transient)

(defvar mov/gptel-quick-models
  '((openwebui . ((quality . gpt-5-chat-latest)
                  (fast    . google/gemini-2.5-flash)))
    (bedrock   . ((quality . claude-3-5-sonnet-latest)
                  (fast    . claude-3-5-haiku-latest))))
  "Quick model presets by backend and intent (quality/fast).")

(defun mov/gptel--backend-key ()
  (cond ((and (boundp 'gptel-openwebui) (eq gptel-backend gptel-openwebui)) 'openwebui)
        ((and (boundp 'gptel-bedrock)   (eq gptel-backend gptel-bedrock))   'bedrock)
        (t nil)))

(defun mov/gptel-set-quick-model (which)
  "Set model to quick preset WHICH (:fast or :quality) for current backend."
  (interactive)
  (let* ((bk (mov/gptel--backend-key))
         (entry (cdr (assq bk mov/gptel-quick-models)))
         (model (cdr (assq (intern (substring (symbol-name which) 1)) entry))))
    (if model
        (progn (setq gptel-model model)
               (message "Set model to %s (%s)" model bk))
      (message "No quick model mapped for %s/%s" bk which))))

(defun mov/gptel-set-fast-model ()
  (interactive)
  (mov/gptel-set-quick-model :fast))

(defun mov/gptel-set-quality-model ()
  (interactive)
  (mov/gptel-set-quick-model :quality))

(transient-define-prefix mov/gptel-transient ()
  "AI actions menu."
  ["Chats"
   ("l" "Chat"           gptel)
   ("r" "Rewrite"        gptel-rewrite)
   ("a" "Lookup"         gptel-lookup)
   ("m" "GPTel menu"     gptel-menu)]
  ["Helpers"
   ("E" "Explain"        mov/gptel-explain-region)
   ("D" "Docstrings"     mov/gptel-docstring-region)
   ("T" "Tests"          mov/gptel-tests-region)
   ("R" "Refactor"       mov/gptel-refactor-region)]
  ["Backend/Model"
   ("s" "Switch backend" gptel-switch-backend)
   ("o" "OpenWebUI"      gptel-switch-to-openwebui)
   ("b" "Bedrock"        gptel-switch-to-bedrock)
   ("q" "Toggle fast/quality" mov/gptel-toggle-fast-quality)
   ("f" "Set fast model" mov/gptel-set-fast-model)
   ("Q" "Set quality model" mov/gptel-set-quality-model)
   ("t" "Toggle temperature" mov/gptel-bump-temperature)]
  ["Context & Files"
   ("A" "Attach from Dired" mov/gptel-add-files-from-dired)
   ("S" "Summarize EWW page" mov/gptel-eww-summarize)
   ("p" "Apply project prompt" mov/gptel-apply-project-system-prompt)
   ("L" "Write dir-locals example" mov/gptel-write-dir-locals-example)])

(define-key ai-map (kbd ".") #'mov/gptel-transient)

;;
;; Smart rename for terminal buffers (EAT/vterm)
;;
(defun mov/terminal--fallback-name ()
  "Heuristic terminal buffer name without AI.
Uses last path segment of `default-directory' and the current
process command, if available."
  (let* ((cwd (abbreviate-file-name (or default-directory "")))
         (proj (file-name-nondirectory (directory-file-name cwd)))
         (proc (get-buffer-process (current-buffer)))
         (cmd (when (process-live-p proc)
                (ignore-errors (car (process-command proc)))))
         (base (string-join (seq-filter #'identity (list proj cmd)) ":")))
    (or (and (stringp base) (not (string-empty-p base)) base)
        (format "term:%s" (or proj "*")))))

(defun mov/gptel-smart-rename-terminal (&optional buffer)
  "Use GPTel to generate a concise name for a terminal BUFFER.
If BUFFER is nil, operate on the current buffer. Falls back to a
heuristic name when GPTel is unavailable. Commands are non-blocking."
  (interactive)
  (let* ((buf (or buffer (current-buffer)))
         (mode (buffer-local-value 'major-mode buf)))
    (unless (memq mode '(eat-mode vterm-mode term-mode))
      (user-error "Not a terminal buffer (eat/vterm/term)"))
    (with-current-buffer buf
      (if (not (featurep 'gptel))
          (let ((name (mov/terminal--fallback-name)))
            (rename-buffer (generate-new-buffer-name name) t)
            (message "Renamed (fallback): %s" (buffer-name)))
        (let* ((cwd (abbreviate-file-name (or default-directory "")))
               (proc (get-buffer-process (current-buffer)))
               (cmd (when (process-live-p proc)
                      (ignore-errors (mapconcat #'identity (process-command proc) " "))))
               (tail (buffer-substring-no-properties
                      (max (point-min) (- (point-max) 4000)) (point-max)))
               (prompt (format "Terminal context:\nMode: %s\nCWD: %s\nCommand: %s\n--- Recent tail ---\n%s\n-------------------\n\nTask: Propose a concise, filesystem-safe buffer name (2–5 words)\nthat helps identify this terminal. Prefer: project/directory name, primary command,\nor clear intent. Constraints: <= 40 chars, ASCII, use hyphens, no slashes, quotes, or punctuation.\nReturn ONLY the name, nothing else."
                                mode cwd (or cmd "") tail))
               (system "You generate short, helpful titles for terminal sessions.\nFocus on the task at hand, directory/project, and command intent.\nExamples: 'nix-build-lab', 'repo-upgrade-homelab', 'git-rebase-hub', 'pytest-auth'"))
          (gptel-request
           prompt
           :system system
           :callback
           (lambda (response _info)
             (if (not (and response (stringp response)))
                 (let ((name (mov/terminal--fallback-name)))
                   (rename-buffer (generate-new-buffer-name name) t)
                   (message "Renamed (fallback): %s" (buffer-name)))
               (let* ((raw (string-trim response))
                      (san (replace-regexp-in-string "[^A-Za-z0-9._-]+" "-" raw))
                      (san (replace-regexp-in-string "-+" "-" san))
                      (san (string-trim san "-"))
                      (san (if (> (length san) 40) (substring san 0 40) san))
                      (final (if (string-empty-p san) (mov/terminal--fallback-name) san)))
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (rename-buffer (generate-new-buffer-name final) t)
                     (message "Renamed: %s" (buffer-name)))))))))))))

(provide 'init-ai)
;;; init-ai.el ends here
