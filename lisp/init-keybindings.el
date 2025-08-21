;;; init-keybindings.el --- Global key bindings -*- lexical-binding: t -*-

;;; Commentary:
;; Global key mappings and custom functions

;;; Code:

;; Global key bindings
(define-prefix-command 'emacs-map)
(global-set-key (kbd "C-c q") 'emacs-map)
(define-key emacs-map (kbd "q") 'save-buffers-kill-terminal)
(define-key emacs-map (kbd "l") 'reload-emacs-config)
(global-set-key (kbd "M-o") 'other-window)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
