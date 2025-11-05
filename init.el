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
(require 'init-config)
(require 'init-utils)

(require 'init-org)
(require 'init-package)
(require 'init-system)
(require 'init-ui)
(require 'init-tabs)
(require 'init-keybindings)

;; Modal editing
(when mov-enable-meow
  (require 'init-meow))

(require 'init-completion)
(require 'init-project)
(require 'init-programming)
(require 'init-misc)

(require 'init-ai)

;; Machine-specific configuration
(require 'init-machine)
(mov-load-machine-config)

(setq projectile-indexing-method 'alien)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
