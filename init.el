;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal Emacs configuration - modular approach

;;; Code:

;; Prefer newer source over stale byte-compiled files
(setq load-prefer-newer t)

;; Load paths and custom file
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

(recentf-mode 1)
(winner-mode 1)
 
;; Load configuration modules
(require 'init-utils)

(require 'init-org)
(require 'init-package)
(require 'init-system)
(require 'init-ui)
(require 'init-tabs)
(require 'init-keybindings)

;; Modal editing (set to nil to disable)
(defvar enable-meow t)
(when enable-meow
  (require 'init-meow))

(require 'init-completion)
(require 'init-project)
(require 'init-programming)
(require 'init-misc)

(require 'init-android)
(require 'init-ai)

(setq projectile-indexing-method 'alien)
(setq projectile-known-projects
      '("/sshx:reef:~/code/nix-configs/"
        "/sshx:reef:~/code/homelab/"
	"~/.emacs.d"))

;; Load work configuration if on work machine
(let ((machine-config-file (expand-file-name "machine-config.el" user-emacs-directory)))
  (when (file-exists-p machine-config-file)
    (load machine-config-file)
    (when (bound-and-true-p is-work-machine)
      (require 'init-work)
      ;;(gptel-switch-to-bedrock)
      ;;(setq gptel-backend gptel-bedrock)
      (message "Work configuration loaded"))))

(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(global-set-key (kbd "M-Q") 'unfill-paragraph)

;;; init.el ends here

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
