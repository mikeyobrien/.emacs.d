
;;; init-android.el --- Android-specific configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Sane defaults for Emacs running on Android devices

;;; Code:

(when (eq system-type 'android)
  ;; Touch-friendly interface
  (setq touch-screen-display-keyboard t)
  (setq use-dialog-box nil)
  (setq inhibit-startup-screen t)
  
  ;; Larger UI elements for touch
  (set-face-attribute 'default nil :height 120)
  (setq-default line-spacing 0.2)
  
  ;; Simplified interface
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  ;;(scroll-bar-mode -1)
  (setq ring-bell-function 'ignore)
  
  ;; Battery-friendly settings
  (setq auto-save-timeout 30)
  (setq auto-save-interval 200)
  (setq gc-cons-threshold (* 20 1024 1024))
  
  ;; Mobile-friendly editing
  (setq-default truncate-lines nil)
  (setq-default word-wrap t)
  (global-visual-line-mode 1)
  
  ;; Simplified completion
  (setq completion-styles '(orderless basic partial-completion))
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  
  ;; Reduce resource usage
  (setq font-lock-maximum-decoration 2)
  (setq jit-lock-stealth-time nil)
  (setq jit-lock-defer-time 0.05)
  
  ;; Android-specific paths
  (when (file-directory-p "/sdcard")
    (setq default-directory "/sdcard/"))
  
  ;; Disable heavy features
  (setq-default indent-tabs-mode nil)
  (setq make-backup-files nil)
  (setq auto-save-default t)

  (add-to-list 'exec-path "/data/data/com.termux/files/usr/bin")
  
  ;; Simple key bindings for virtual keyboard
  (global-set-key (kbd "C-x C-s") 'save-buffer)
  (global-set-key (kbd "C-x C-f") 'find-file)
  (global-set-key (kbd "C-x b") 'switch-to-buffer)
  (global-set-key (kbd "C-g") 'keyboard-quit)
  
  ;; Android hardware button bindings
  ;; Volume gup -> M-x, Volume down -> find-file
  (global-set-key [volume-up] 'execute-extended-command)
  (global-set-key [volume-down] 'meow-escape-or-normal-modal))

(provide 'init-android)
;;; init-android.el ends here
