;;; init-misc.el --- Miscellaneous utilities -*- lexical-binding: t -*-

;;; Commentary:
;; Which-key, terminal, EPUB reader, and other utilities

;;; Code:

;; Auto-revert mode - automatically refresh buffers when files change externally
(use-package autorevert
  :ensure nil  ; built-in package
  :config
  (global-auto-revert-mode 1)
  :custom
  (auto-revert-interval 1)  ; Check for changes every second
  (auto-revert-check-vc-info t)  ; Also check version control info
  (auto-revert-verbose nil)  ; Don't show messages when reverting
  (global-auto-revert-non-file-buffers t))  ; Also revert non-file buffers like Dired

;; Which-key (key binding help)
(use-package which-key
  :demand t
  :init (which-key-mode 1)
  :custom (which-key-idle-delay 0.4))

;; Visual undo tree
(use-package vundo
  :bind ("C-x u" . vundo))

;; Better help buffers
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

;; Disable mouse
(use-package disable-mouse)

;; EPUB reader
(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; History binding: use Consult everywhere
  (define-key eshell-mode-map (kbd "C-r") #'consult-history)

  ;; Only define Evil-specific bindings if Evil is present
  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell))

(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'robbyrussell))

;; Terminal emulator
(use-package eat
  :hook
  ((eat-mode . my/eat-disable-line-numbers)
   (eat-mode . my/eat-setup-meow)
   (eshell-mode . eat-eshell-mode))
  :config
  (setq eat-enable-mouse nil
        eat-kill-buffer-on-exit t)

  (define-key eat-mode-map (kbd "C-c C-n") #'mov/gptel-smart-rename-terminal))

(defun my/eat-disable-line-numbers ()
  (interactive)
  "Turn off line numbers inside EAT buffers."
  (setq-local display-line-numbers nil)
  (display-line-numbers-mode -1))

(defun my/eat-setup-meow ()
  (when (fboundp 'my/eat--setup-meow-hooks)
    (my/eat--setup-meow-hooks)))

;; Bind the same renamer in vterm if available
(with-eval-after-load 'vterm
  (when (boundp 'vterm-mode-map)
    (define-key vterm-mode-map (kbd "C-c C-n") #'mov/gptel-smart-rename-terminal)))


(use-package pdf-tools
  
  :config
  (pdf-tools-install)     ;; builds and activates
  (setq-default pdf-view-display-size 'fit-page)
  ;; open PDFs in pdf-view-mode instead of doc-view
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode)))

;; Optional extras
(use-package saveplace-pdf-view
  :after pdf-tools
  :config
  (save-place-mode 1))   ;; remembers last viewed page


(provide 'init-misc)

;;; init-misc.el ends here
