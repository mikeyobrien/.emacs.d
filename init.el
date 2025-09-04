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
(require 'init-completion)
(require 'init-project)
(require 'init-programming)
(require 'init-misc)

(require 'init-android)
(require 'init-ai)

(customize-set-variable 'tramp-default-method "sshx")
;; TRAMP performance optimizations for Projectile
(setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=10"
      tramp-completion-reread-directory-timeout 10
      tramp-copy-size-limit 10000
      remote-file-name-inhibit-cache nil
      tramp-verbose 1)

;; Load work configuration if on work machine
(when (and (file-exists-p (expand-file-name "machine-config.el" user-emacs-directory))
           (progn (require 'machine-config nil t)
                  (bound-and-true-p is-work-machine)))
  (require 'init-work)
  (message "Work configuration loaded"))


;;; init.el ends here

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
