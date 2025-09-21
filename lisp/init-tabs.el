;;; init-tabs.el --- Tab management and keybinds -*- lexical-binding: t -*-

;;; Commentary:
;; Enable built-in tab-bar and provide ergonomic keybindings for tab
;; creation, navigation, closing, and renaming. Integrates with Meow
;; leader keys if Meow is present.

;;; Code:

;; Enable tab-bar and set simple defaults
(setq tab-bar-show 1)                    ; only show when >1 tab
(setq tab-bar-new-tab-to 'right)         ; new tabs open to the right
(tab-bar-mode 1)

;; Convenience functions for selecting tabs by number
(defun mov/tab-select (n)
  "Select tab number N (1-based)."
  (interactive "p")
  (tab-bar-select-tab n))

(defun mov/tab-select-1 () (interactive) (mov/tab-select 1))
(defun mov/tab-select-2 () (interactive) (mov/tab-select 2))
(defun mov/tab-select-3 () (interactive) (mov/tab-select 3))
(defun mov/tab-select-4 () (interactive) (mov/tab-select 4))
(defun mov/tab-select-5 () (interactive) (mov/tab-select 5))
(defun mov/tab-select-6 () (interactive) (mov/tab-select 6))
(defun mov/tab-select-7 () (interactive) (mov/tab-select 7))
(defun mov/tab-select-8 () (interactive) (mov/tab-select 8))
(defun mov/tab-select-9 () (interactive) (mov/tab-select 9))

;; Move current tab left/right with fallback across Emacs versions
(defun mov/tab-move-left ()
  "Move the current tab one position to the left."
  (interactive)
  (cond
   ((fboundp 'tab-bar-move-tab)
    (tab-bar-move-tab -1))
   ((fboundp 'tab-bar-move-tab-backward)
    (tab-bar-move-tab-backward))
   (t (user-error "Tab move not supported in this Emacs"))))

(defun mov/tab-move-right ()
  "Move the current tab one position to the right."
  (interactive)
  (cond
   ((fboundp 'tab-bar-move-tab)
    (tab-bar-move-tab +1))
   ((fboundp 'tab-bar-move-tab-forward)
    (tab-bar-move-tab-forward))
   (t (user-error "Tab move not supported in this Emacs"))))

;; Global keybindings (terminal-friendly prefix under C-c t)
(define-prefix-command 'mov/tabs-prefix)
(global-set-key (kbd "C-c t") 'mov/tabs-prefix)
(global-set-key (kbd "C-c t n") #'tab-bar-new-tab)
(global-set-key (kbd "C-c t d") #'tab-bar-close-tab)
(global-set-key (kbd "C-c t o") #'tab-bar-close-other-tabs)
(global-set-key (kbd "C-c t r") #'tab-bar-rename-tab)
(global-set-key (kbd "C-c t [") #'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-c t ]") #'tab-bar-switch-to-next-tab)
(global-set-key (kbd "C-c t <") #'mov/tab-move-left)
(global-set-key (kbd "C-c t >") #'mov/tab-move-right)
(global-set-key (kbd "C-c t 1") #'mov/tab-select-1)
(global-set-key (kbd "C-c t 2") #'mov/tab-select-2)
(global-set-key (kbd "C-c t 3") #'mov/tab-select-3)
(global-set-key (kbd "C-c t 4") #'mov/tab-select-4)
(global-set-key (kbd "C-c t 5") #'mov/tab-select-5)
(global-set-key (kbd "C-c t 6") #'mov/tab-select-6)
(global-set-key (kbd "C-c t 7") #'mov/tab-select-7)
(global-set-key (kbd "C-c t 8") #'mov/tab-select-8)
(global-set-key (kbd "C-c t 9") #'mov/tab-select-9)

;; Optional: familiar next/prev tab keys (may not work in all terminals)
(when (display-graphic-p)
  (global-set-key (kbd "<C-tab>") #'tab-bar-switch-to-next-tab)
  (global-set-key (kbd "<C-S-iso-lefttab>") #'tab-bar-switch-to-prev-tab))

;; Meow leader integration (SPC t ...)
(with-eval-after-load 'meow
  (when (fboundp ' meow-leader-define-key)
    (meow-leader-define-key
     '("t n" . tab-bar-new-tab)
     '("t d" . tab-bar-close-tab)
     '("t o" . tab-bar-close-other-tabs)
     '("t r" . tab-bar-rename-tab)
     '("t [" . tab-bar-switch-to-prev-tab)
     '("t ]" . tab-bar-switch-to-next-tab)
     '("t <" . mov/tab-move-left)
     '("t >" . mov/tab-move-right)
     '("t 1" . mov/tab-select-1)
     '("t 2" . mov/tab-select-2)
     '("t 3" . mov/tab-select-3)
     '("t 4" . mov/tab-select-4)
     '("t 5" . mov/tab-select-5)
     '("t 6" . mov/tab-select-6)
     '("t 7" . mov/tab-select-7)
     '("t 8" . mov/tab-select-8)
     '("t 9" . mov/tab-select-9))
    ))

(provide 'init-tabs)

;;; init-tabs.el ends here
