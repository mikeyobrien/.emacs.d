;;; integrations.el --- AI integrations with other modes -*- lexical-binding: t -*-

;;; Commentary:
;; Dired, EWW, terminal, and project integrations

;;; Code:

(require 'gptel)
(require 'project)

;; Dired integration
(with-eval-after-load 'dired
  (defun mov/gptel-add-files-from-dired ()
    "Attach marked files in Dired to the current gptel chat."
    (interactive)
    (let ((files (dired-get-marked-files)))
      (dolist (f files)
        (gptel-add-file f))
      (message "Attached %d file(s) to GPTel" (length files)))))

;; EWW integration
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
       :buffer (get-buffer-create "*gptel-eww*")))))

;; Terminal integration
(defun mov/terminal--fallback-name ()
  "Heuristic terminal buffer name without AI."
  (let* ((cwd (abbreviate-file-name (or default-directory "")))
         (proj (file-name-nondirectory (directory-file-name cwd)))
         (proc (get-buffer-process (current-buffer)))
         (cmd (when (process-live-p proc)
                (ignore-errors (car (process-command proc)))))
         (base (string-join (seq-filter #'identity (list proj cmd)) ":")))
    (or (and (stringp base) (not (string-empty-p base)) base)
        (format "term:%s" (or proj "*")))))

(defun mov/gptel-smart-rename-terminal (&optional buffer)
  "Use GPTel to generate a concise name for a terminal BUFFER."
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

;; Project system prompt
(defun mov/gptel--read-file-first-k (file k)
  "Read first K characters from FILE."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-substring-no-properties (point-min) (min (point-max) (+ (point-min) k))))))

(defun mov/gptel--project-root ()
  "Get current project root."
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

(add-hook 'gptel-mode-hook #'mov/gptel-apply-project-system-prompt)

(provide 'integrations)
;;; integrations.el ends here
