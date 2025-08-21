;;; init-project.el --- Project management and version control -*- lexical-binding: t -*-

;;; Commentary:
;; Projectile and Magit configuration

;;; Code:

;; Projectile
(use-package projectile
  :ensure t
  :bind
  ("C-c p" . 'projectile-command-map)
  :config
  (when (executable-find "rg")
    (setq projectile-generic-command "rg --files --color=never --hidden")
    (setq projectile-grep-command "rg --color=never --no-heading --line-number --smart-case . -e ")
    (setq projectile-use-git-grep nil)))

;; Magit
(use-package magit
  :bind
  ("C-x g". magit-status))

(provide 'init-project)
;;; init-project.el ends here
