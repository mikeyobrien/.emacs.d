;;; init-ui.el --- UI and appearance -*- lexical-binding: t -*-

;;; Commentary:
;; Theme, modeline, fonts, and window management

;;; Code:

;; Theme and modeline
(use-package doom-themes
  :config
  (load-theme 'doom-one t))
(use-package all-the-icons
  :ensure t)
(use-package nerd-icons)
(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

;; Font configuration
(defun setup-fonts ()
  "Set up fonts, preferring Fira Code Nerd Font if available."
  (let ((font-height 150))
    (cond
     ;; Check for Fira Code Nerd Font
     ((find-font (font-spec :name "FiraCode Nerd Font"))
      (set-face-attribute 'default nil
                          :font "FiraCode Nerd Font"
                          :height font-height)
      (set-fontset-font t 'symbol "FiraCode Nerd Font" nil 'prepend)
      (message "Using FiraCode Nerd Font"))
     ;; Fallback to regular Fira Code
     ((find-font (font-spec :name "Fira Code"))
      (set-face-attribute 'default nil
                          :font "Fira Code"
                          :height font-height)
      (message "Using Fira Code"))
     (t
      (set-face-attribute 'default nil :height font-height)
      (message "Fira Code fonts not found, using default font")))))

;; Apply font settings
(setup-fonts)

;; Enable ligatures for Fira Code if available
(when (and (fboundp 'mac-auto-operator-composition-mode)
           (member "FiraCode Nerd Font" (font-family-list)))
  (mac-auto-operator-composition-mode t))

;; Simplified interface
(menu-bar-mode -1)
(tool-bar-mode -1)


(setq ring-bell-function 'ignore)
  
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
