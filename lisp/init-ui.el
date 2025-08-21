;;; init-ui.el --- UI and appearance -*- lexical-binding: t -*-

;;; Commentary:
;; Theme, modeline, fonts, and window management

;;; Code:

;; Theme and modeline
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(use-package nerd-icons)
(use-package doom-modeline
  :init
  (doom-modeline-mode))

;; Font size
(set-face-attribute 'default nil :height 150)

;; Window management
(use-package ace-window
  :ensure t
  :bind
  ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)))

;; Buffer display rules
(setq display-buffer-alist
      '(("*Python*"
         (display-buffer-reuse-window
          display-buffer-at-bottom)
         (window-height . 0.3))))

(provide 'init-ui)
;;; init-ui.el ends here
