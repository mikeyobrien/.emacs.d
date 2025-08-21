;;; init-system.el --- System integration -*- lexical-binding: t -*-

;;; Commentary:
;; Path, environment, auto-save, and file management

;;; Code:

;; Path and environment
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Auto-revert and auto-save
(global-auto-revert-mode 1)
(setq auto-revert-check-vc-info t)
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

;; File backups and auto-saves
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-saves/") t)))
(make-directory (concat user-emacs-directory "auto-saves/") t)

(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "backups/"))))
(make-directory (concat user-emacs-directory "backups/") t)

(provide 'init-system)
;;; init-system.el ends here
