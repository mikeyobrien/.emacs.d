;;; init-system.el --- System integration -*- lexical-binding: t -*-

;;; Commentary:
;; Path, environment, auto-save, and file management

;;; Code:

;; Path and environment
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Improve subprocess throughput for LSP/formatters
(setq read-process-output-max (* 1 1024 1024)
      process-adaptive-read-buffering t)

;; Host-specific configurations
(defconst is-g14-host
  (string-match-p "g14\\|GA403WR" (system-name))
  "Check if running on G14 laptop.")

;; Terminal emulator - prefer EAT globally; keep vterm only on G14
(when is-g14-host
  (use-package vterm
    :ensure t
    :bind (("C-c T" . vterm-other-window))
    :hook ((vterm-mode . (lambda () (display-line-numbers-mode -1)))
           (vterm-mode . (lambda ()
                           (setq-local cursor-type '(bar . 2)))))
    :config
    (setq vterm-max-scrollback 10000
          vterm-buffer-name-string "vterm %s"
          vterm-kill-buffer-on-exit t)))

;; Auto-save on focus out
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

;; File backups and auto-saves
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t)))
(make-directory (expand-file-name "auto-saves/" user-emacs-directory) t)

(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backups/" user-emacs-directory))))
(make-directory (expand-file-name "backups/" user-emacs-directory) t)

;; Session QoL
(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1)
  :custom
  (history-length 300)
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring)))

(use-package saveplace
  :ensure nil
  :init (save-place-mode 1))

;; Smooth GC during interaction
(use-package gcmh
  :init (gcmh-mode 1)
  :custom
  (gcmh-idle-delay 1)
  (gcmh-high-cons-threshold (* 64 1024 1024)))

(provide 'init-system)
;;; init-system.el ends here
