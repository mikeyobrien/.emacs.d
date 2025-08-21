;;; init-misc.el --- Miscellaneous utilities -*- lexical-binding: t -*-

;;; Commentary:
;; Which-key, terminal, EPUB reader, and other utilities

;;; Code:

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
