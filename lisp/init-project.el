;;; init-project.el --- Project management and version control -*- lexical-binding: t -*-

;;; Commentary:
;; Projectile and Magit configuration

;;; Code:

;; Projectile
(use-package projectile
  :ensure t
  :bind
  ("C-c p" . 'projectile-command-map)
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

;; Magit
(use-package magit
  :bind
  ("C-x g". magit-status))

(provide 'init-project)
;;; init-project.el ends here
