;;; init-ui.el --- UI and appearance -*- lexical-binding: t -*-

;;; Commentary:
;; Theme, modeline, fonts, and window management

;;; Code:

;; Theme and modeline
(use-package doom-themes
  :config
  (load-theme 'doom-ayu-dark t))
(use-package all-the-icons
  :ensure t)
(use-package nerd-icons)
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :custom
  (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 12 :weight 'regular))
  (setq nerd-icons-font-family "JetBrainsMono Nerd Font")
  (setq doom-symbol-font (font-spec :family "JetBrainsMono Nerd Font" :size 11)))

;; Font configuration
(defun setup-fonts ()
  "Set up fonts, preferring IosevkaTerm Nerd Font if available."
  (let ((font-height (if (string-equal (system-name) "g14") 10 150)))
    (cond
     ;; Check for IosevkaTerm Nerd Font
     ((find-font (font-spec :name "IosevkaTerm Nerd Font"))
      (set-face-attribute 'default nil
                          :font "IosevkaTerm Nerd Font"
                          :height font-height)
      (set-fontset-font t 'symbol "IosevkaTerm Nerd Font" nil 'prepend)
      (message "Using IosevkaTerm Nerd Font"))
     ;; Fallback to Fira Code Nerd Font
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
      (message "IosevkaTerm/Fira Code fonts not found, using default font")))))

;; Apply font settings
(setup-fonts)

;; Enable ligatures if available
(when (and (fboundp 'mac-auto-operator-composition-mode)
           (or (member "IosevkaTerm Nerd Font" (font-family-list))
               (member "FiraCode Nerd Font" (font-family-list))))
  (mac-auto-operator-composition-mode t))

;; Simplified interface
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (display-graphic-p)
  (scroll-bar-mode -1))

(setq ring-bell-function 'ignore)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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
