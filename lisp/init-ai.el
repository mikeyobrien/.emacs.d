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
  :ensure t
  :demand t
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
		gpt-oss:120b
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
         (special-mode)
         (display-buffer (current-buffer)
                         `((display-buffer-in-side-window)
                           (side . bottom)
                           (window-height . ,#'fit-window-to-buffer))))))))

(defun mov/gptel-insert ()
  (interactive)
  (set-mark (point))
  (insert "\n")
  (exchange-point-and-mark)
  (gptel-rewrite))
  

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
        gptel-disabled-modes '(markdown-mode))
  (add-hook 'gptel-mode-hook
            (lambda ()
              (setenv "AWS_PROFILE" "cline"))))

;; Backend switching functions (outside use-package)
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

;; MCP (Model Context Protocol)
(use-package mcp
  :ensure t
  :after gptel
  :config
  (let* ((filesystem-root "/Users/mobrienv/")
         (servers (list (cons "fetch" '(:command "uvx" :args ("mcp-server-fetch")))
			(cons "builder-mcp" '(:command "builder-mcp"))
                        (cons "filesystem"
                              `(:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" ,filesystem-root))))))
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
(define-key ai-map (kbd "?") 'gptel-quick)

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
  "Explain the selected code/text or current buffer and show result in minibuffer."
  (interactive)
  (let ((content (mov/gptel--region-or-buffer-string)))
    (gptel-request
     (format "Explain the following content clearly and concisely.\n\n%s" content)
     :system "You are a senior engineer. Provide a precise, succinct explanation, include gotchas and edge cases when relevant."
     :callback (lambda (response)
                 (when response
                   (message "%s" response))))))

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

;; gptel-quick
(require 'thingatpt)

(declare-function pdf-view-active-region-p "pdf-view")
(declare-function pdf-view-active-region-text "pdf-view")

(defcustom gptel-quick-display (and (fboundp 'posframe-workable-p)
                                    'posframe)
  "How to display `gptel-quick' results.

Set to `posframe' to use posframe, if available.  Any other value
will cause the result to be displayed in the echo area."
  :type '(choice
          (const :tag "In posframe" posframe)
          (const :tag "Echo area" nil))
  :group 'gptel)

(defvar gptel-quick-system-message
  (lambda (count)
    (format "Explain in %d words or fewer." count))
  "System message for `gptel-quick'.  It is a function called with
one argument, the desired word count -- see
`gptel-quick-word-count'.

WARNING: This variable is experimental and the calling convention is
subject to change in the future.")

(defvar gptel-quick-word-count 12
  "Approximate word count of LLM summary.")
(defvar gptel-quick-timeout 10
  "Time in seconds before dismissing the summary.")
(defvar gptel-quick-use-context nil
  "Whether to use gptel's active context.

This can include other regions, buffers or files added by
`gptel-add'.")
(defvar gptel-quick-backend nil
  "Set `gptel-quick-backend' to use a dedicated model. Require
`gptel-quick-model' to be configured.")
(defvar gptel-quick-model nil
  "Set `gptel-quick-model' to use a dedicated model. Must be one of
`gptel-quick-backend''s models. Require `gptel-quick-backend' to
be configured.")

;;;###autoload
(defun gptel-quick (query-text &optional count)
  "Explain or summarize region or thing at point with an LLM.

QUERY-TEXT is the text being explained.  COUNT is the approximate
word count of the response."
  (interactive
   (list (cond
          ((use-region-p) (buffer-substring-no-properties (region-beginning)
                                                          (region-end)))
          ((and (derived-mode-p 'pdf-view-mode)
                (pdf-view-active-region-p))
           (mapconcat #'identity (pdf-view-active-region-text) "\n\n"))
          (t (thing-at-point 'sexp)))
         current-prefix-arg))

  (when (xor gptel-quick-backend gptel-quick-model)
    (error "gptel-quick-backend and gptel-quick-model must be both set or unset"))

  (let* ((count (or count gptel-quick-word-count))
         (gptel-max-tokens (floor (+ (sqrt (length query-text))
                                     (* count 2.5))))
         (gptel-use-curl)
         (gptel-use-context (and gptel-quick-use-context 'system))
         (gptel-backend (or gptel-quick-backend gptel-backend))
         (gptel-model (or gptel-quick-model gptel-model)))
    (gptel-request query-text
      :system (funcall gptel-quick-system-message count)
      :context (list query-text count
                     (posn-at-point (and (use-region-p) (region-beginning))))
      :callback #'gptel-quick--callback-posframe)))

;; From (info "(elisp) Accessing Mouse")
(defun gptel-quick--frame-relative-coordinates (position)
  "Return frame-relative coordinates from POSITION.

POSITION is assumed to lie in a window text area."
  (let* ((x-y (posn-x-y position))
         (window (posn-window position))
         (edges (window-inside-pixel-edges window)))
    (cons (+ (or (car x-y) 0) (car edges))
          (+ (or (cdr x-y) 0) (cadr edges)))))

(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")

(defun gptel-quick--callback-posframe (response info)
  "Show RESPONSE appropriately, in a popup if possible.

Uses the buffer context from INFO.  Set up a transient map for
quick actions on the popup."
  (pcase response
    ('nil (message "Response failed with error: %s" (plist-get info :status)))
    ((pred stringp)
     (pcase-let ((`(,query ,count ,pos) (plist-get info :context)))
       (gptel-quick--update-posframe response pos)
       (cl-flet ((clear-response () (interactive)
                   (and (eq gptel-quick-display 'posframe)
                        (fboundp 'posframe-hide)
                        (posframe-hide " *gptel-quick*")))
                 (more-response  () (interactive)
                   (gptel-quick--update-posframe
                    "...generating longer summary..." pos)
                   (gptel-quick query (* count 4)))
                 (copy-response  () (interactive) (kill-new response)
                   (message "Copied summary to kill-ring."))
                 (create-chat () (interactive)
                   (gptel (generate-new-buffer-name "*gptel-quick*") nil
                          (concat query "\n\n"
                                  (propertize response 'gptel 'response) "\n\n")
                          t)))
         (set-transient-map
          (let ((map (make-sparse-keymap)))
            (define-key map [remap keyboard-quit] #'clear-response)
            (define-key map (kbd "+") #'more-response)
            (define-key map [remap kill-ring-save] #'copy-response)
            (define-key map (kbd "M-RET") #'create-chat)
            map)
          nil #'clear-response nil gptel-quick-timeout))))
    (`(tool-call . ,tool-calls)
     (gptel--display-tool-calls tool-calls info 'minibuffer))))

(defun gptel-quick--update-posframe (response pos)
  "Show RESPONSE at in a posframe (at POS) or the echo area."
  (if (and (display-graphic-p)          ;posframe is not terminal-compatible
           (eq gptel-quick-display 'posframe)
           (require 'posframe nil t))
      (let ((fringe-indicator-alist nil)
            (coords) (poshandler))
        (if (and pos (not (equal (posn-x-y pos) '(0 . 0))))
            (setq coords (gptel-quick--frame-relative-coordinates pos))
          (setq poshandler #'posframe-poshandler-window-center))
        (posframe-show " *gptel-quick*"
                       :string response
                       :position coords
                       :border-width 2
                       :border-color (face-attribute 'vertical-border :foreground)
                       :initialize #'visual-line-mode
                       :poshandler poshandler
                       :left-fringe 8
                       :right-fringe 8
                       :min-width 36
                       :max-width fill-column
                       :min-height 1
                       :timeout gptel-quick-timeout))
    (message response)))

(define-key ai-map (kbd "?") #'gptel-quick)

(defun mov/gptel-complete (prompt)
  "Send PROMPT to GPTel and insert response at point."
  (interactive (list (read-string "Complete with AI: ")))
  (when (string-empty-p prompt)
    (user-error "A prompt is required"))
  (gptel-request
   prompt
   :callback
   (lambda (response _info)
     (when (and response (stringp response))
       (save-excursion
         (insert response))))))

;; optional binding
(define-key ai-map (kbd "C") #'mov/gptel-complete)

(provide 'init-ai)
;;; init-ai.el ends here
