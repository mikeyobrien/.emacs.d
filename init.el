;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal Emacs configuration - modular approach

;;; Code:

;; Load paths and custom file
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

;; Load configuration modules
(require 'init-utils)
(require 'init-org)
(require 'init-package)
(require 'init-system)
(require 'init-ui)
(require 'init-completion)
(require 'init-project)
(require 'init-programming)
(require 'init-misc)
(require 'init-keybindings)
(require 'init-android)
(require 'init-ai)


;; Load work configuration if on work machine
(when (and (file-exists-p (expand-file-name "machine-config.el" user-emacs-directory))
           (progn (require 'machine-config nil t)
                  (bound-and-true-p is-work-machine)))
  (require 'init-work)
  (message "Work configuration loaded"))

;;; init.el ends here
