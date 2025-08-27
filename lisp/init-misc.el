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
(which-key-mode 1)

;; Disable mouse
(use-package disable-mouse)

;; EPUB reader
(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; Terminal emulator
(use-package eat
  :bind
  ("C-c t" . eat)
  :custom
  ;; Add settings so eat behaves better with meow mode
  (with-eval-after-load 'eat
    (with-eval-after-load 'meow
      (defun my/eat-meow-integration ()
        (setq eat-enable-mouse nil)
        (setq eat-kill-buffer-on-exit t)
        (add-hook 'eat-exit-hook #'meow--apply-input-method))
      
      (add-hook 'eat-mode-hook #'my/eat-meow-integration)
      
      (define-key eat-mode-map (kbd "C-c C-c") #'meow-normal-mode)
      (define-key eat-mode-map (kbd "C-c C-i") #'meow-insert-mode)
      (define-key eat-mode-map (kbd "C-c C-v") #'meow-visual-mode)
      (define-key eat-mode-map (kbd "C-c C-e") #'meow-insert-exit))))



(provide 'init-misc)

;;; init-misc.el ends here
