;;; init-system.el --- System integration -*- lexical-binding: t -*-

;;; Commentary:
;; Path, environment, auto-save, and file management

;;; Code:

;; Path and environment
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Host-specific configurations
(defconst is-g14-host
  (string-match-p "g14\\|GA403WR" (system-name))
  "Check if running on G14 laptop.")

;; Terminal emulator - only on G14
(when is-g14-host
  (use-package vterm
    :ensure t
    :bind (("C-c t" . vterm)
           ("C-c T" . vterm-other-window))
    :hook (vterm-mode . (lambda () (display-line-numbers-mode -1)))
    :config
    (setq vterm-max-scrollback 10000
          vterm-buffer-name-string "vterm %s"
          vterm-kill-buffer-on-exit t)))

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
