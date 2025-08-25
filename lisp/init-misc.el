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
  ("C-c t" . eat))

(provide 'init-misc)
;;; init-misc.el ends here
