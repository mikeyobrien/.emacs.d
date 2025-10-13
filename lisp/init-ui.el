;;; init-ui.el --- UI settings -*- lexical-binding: t -*-

;;; Commentary:
;; Interface settings and window management

;;; Code:

;; Load UI components
(require 'init-theme)
(require 'init-modeline)
(require 'init-fonts)

;; Simplified interface
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (display-graphic-p)
  (scroll-bar-mode -1))

;; Disable GUI dialogs
(setq use-dialog-box nil
      use-file-dialog nil
      ring-bell-function 'ignore)

;; Better scrolling
(setq scroll-margin 3
      scroll-conservatively 101
      scroll-preserve-screen-position t)

;; Show matching parens
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Window management
(use-package ace-window
  :bind ("M-o" . ace-window)
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
