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
  (when (featurep 'evil)
    (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
    (when (fboundp 'evil-normalize-keymaps)
      (evil-normalize-keymaps)))

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
  :bind
  ("C-c t" . eat)
  :hook
  ((eat-mode . my/eat-disable-line-numbers)
   (eat-mode . my/eat-setup-meow))
  :config
  (setq eat-enable-mouse nil
        eat-kill-buffer-on-exit t)

  (define-key eat-mode-map (kbd "C-c C-c") #'meow-normal-mode)
  (define-key eat-mode-map (kbd "C-c C-i") #'meow-insert-mode)
  (define-key eat-mode-map (kbd "C-c C-v") #'meow-visual-mode)
  (define-key eat-mode-map (kbd "C-c C-e") #'meow-insert-exit)
  ;; Smart rename via GPTel
  (define-key eat-mode-map (kbd "C-c C-n") #'mov/gptel-smart-rename-terminal)

  (with-eval-after-load 'meow
    (defun my/eat--meow-enter-insert ()
      (when (derived-mode-p 'eat-mode)
        (eat-char-mode)))

    (defun my/eat--meow-exit-insert ()
      (when (derived-mode-p 'eat-mode)
        (eat-emacs-mode)))

    (defun my/eat--teardown-meow-hooks ()
      (remove-hook 'meow-insert-enter-hook #'my/eat--meow-enter-insert t)
      (remove-hook 'meow-insert-exit-hook #'my/eat--meow-exit-insert t)
      (remove-hook 'kill-buffer-hook #'my/eat--teardown-meow-hooks t))

    (defun my/eat--setup-meow-hooks ()
      (add-hook 'meow-insert-enter-hook #'my/eat--meow-enter-insert nil t)
      (add-hook 'meow-insert-exit-hook #'my/eat--meow-exit-insert nil t)
      (add-hook 'kill-buffer-hook #'my/eat--teardown-meow-hooks nil t)
      (if (bound-and-true-p meow-insert-mode)
          (my/eat--meow-enter-insert)
        (my/eat--meow-exit-insert)))))

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



(provide 'init-misc)

;;; init-misc.el ends here
