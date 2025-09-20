;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal Emacs configuration - modular approach

;;; Code:

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

(customize-set-variable 'tramp-default-method "sshx")
(add-to-list 'tramp-connection-properties
             (list (regexp-quote "/ssh:reef:")
                   "remote-shell" "/run/current-system/sw/bin/bash"))

;; TRAMP performance optimizations for Projectile
(setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=10"
      tramp-completion-reread-directory-timeout 10
      tramp-copy-size-limit 10000
      remote-file-name-inhibit-cache nil
      tramp-verbose 1)

(add-to-list 'tramp-default-method-alist
             '("\\`reef\\'" "\\`.*\\'" "sshx"))

;; Add handy directory abbreviations
(setq directory-abbrev-alist
      '(("^/reef:nix$" . "/sshx:reef:~/code/nix-configs")
        ("^/reef:lab$" . "/sshx:reef:~/code/homelab")))

(setq projectile-indexing-method 'alien)
(setq projectile-known-projects
      '("/sshx:reef:~/code/nix-configs/"
        "/sshx:reef:~/code/homelab/"))

;; Load work configuration if on work machine
(let ((machine-config-file (expand-file-name "machine-config.el" user-emacs-directory)))
  (when (file-exists-p machine-config-file)
    (load machine-config-file)
    (when (bound-and-true-p is-work-machine)
      (require 'init-work)
      (setq gptel-backend gptel-bedrock)
      (message "Work configuration loaded"))))

;;; init.el ends here

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
