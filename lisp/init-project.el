;;; init-project.el --- Project management and version control -*- lexical-binding: t -*-

;;; Commentary:
;; Projectile and Magit configuration

;;; Code:

;; Projectile
(use-package projectile
  
  :init
  (progn
    (setq projectile-file-exists-remote-cache-expire nil)
    (setq projectile-mode-line '(:eval (format " Projectile[%s]" (projectile-project-name))))
    (setq projectile-globally-ignored-directories
          (quote
           (".idea" ".eunit" ".git" ".hg" ".svn" ".fslckout" ".bzr" "_darcs" ".tox" "build" "target"))))

  :config
  ;; Performance optimizations for TRAMP
  (setq projectile-enable-caching t
        projectile-file-exists-remote-cache-expire 120
        projectile-require-project-root 'prompt
        projectile-completion-system 'default)
  
  ;; Remote project support
  (setq projectile-mode-line-prefix " Proj"
        projectile-dynamic-mode-line t)
  
  ;; Efficient commands for remote projects
  (when (executable-find "rg")
    (setq projectile-generic-command "rg --files --color=never --hidden")
    (setq projectile-grep-command "rg --color=never --no-heading --line-number --smart-case . -e ")
    (setq projectile-use-git-grep nil))
  
  ;; Git command optimizations for remote projects
  (setq projectile-git-command "git ls-files -zco --exclude-standard"))

;; Consult integration for Projectile (keeps Vertico/Consult stack consistent)
(use-package consult-projectile
  :after (projectile consult))

;; Custom function to run eat terminal at bottom
(defun projectile-run-eat ()
  "Run eat terminal in project root at bottom of window."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (let ((display-buffer-alist
           '(("\\*eat\\*"
              (display-buffer-at-bottom)
              (window-height . 0.3)))))
      (eat))))

;; Toggle project-specific vterm
(defun mov/project-vterm-toggle ()
  "Toggle a project-specific vterm buffer.
If the project vterm buffer exists and is visible, switch back to previous buffer.
If it exists but is not visible, switch to it.
If it doesn't exist, create it at project root.
Falls back to eat if vterm is not available."
  (interactive)
  (let* ((project-root (condition-case nil
                           (projectile-project-root)
                         (error default-directory)))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (vterm-available (fboundp 'vterm))
         (buffer-name (if vterm-available
                          (format "*vterm %s*" project-name)
                        (format "*eat %s*" project-name)))
         (vterm-buffer (get-buffer buffer-name)))

    (cond
     ;; Buffer exists and is visible - toggle back to previous buffer
     ((and vterm-buffer (get-buffer-window vterm-buffer))
      (switch-to-buffer (other-buffer vterm-buffer)))

     ;; Buffer exists but not visible - switch to it
     (vterm-buffer
      (switch-to-buffer vterm-buffer))

     ;; Buffer doesn't exist - create it
     (t
      (let ((default-directory project-root))
        (if vterm-available
            (progn
              (vterm buffer-name)
              ;; Rename buffer if vterm created a different name
              (when (not (string= (buffer-name) buffer-name))
                (rename-buffer buffer-name)))
          ;; Fall back to eat
          (let ((eat-buffer-name buffer-name))
            (eat)
            (rename-buffer buffer-name t))))))))

;; Keybinding for project vterm toggle
;; Bind to both Projectile's command map (C-c p t) and Emacs project map (C-x p t)
(with-eval-after-load 'projectile
  (define-key projectile-command-map (kbd "t") #'mov/project-vterm-toggle))

;; Also bind to the built-in project prefix map for C-x p t
(with-eval-after-load 'project
  (define-key project-prefix-map (kbd "t") #'mov/project-vterm-toggle))

;; Magit
(use-package magit
  :bind
  ("C-x g". magit-status))

(provide 'init-project)
;;; init-project.el ends here
