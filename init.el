;; -*- lexical-binding: t; -*-
(require 'package)

(which-key-mode 1)
 
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package disable-mouse
  :init
  (global-disable-mouse-mode))

(define-prefix-command 'emacs-map)
(global-set-key (kbd "C-c q") 'emacs-map)

(define-key emacs-map (kbd "q") 'save-buffers-kill-terminal)

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 250)
  (corfu-auto-prefix 250)
  :init
  (global-corfu-mode))

(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

(use-package embark
  :bind
  (("C-.". embark-act)
   ("M-.". embark-dwim)
   ("C-,". embark-act-all)))


(use-package magit
  :bind
  ("C-x g". magit-status))

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(use-package nerd-icons)
(use-package doom-modeline
  :init
  (doom-modeline-mode))

(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(set-face-attribute 'default nil :height 200)

(global-set-key (kbd "M-o") 'other-window)

(use-package eat)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)  ; or 'auto for no prompting
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-saves/") t)))
(make-directory (concat user-emacs-directory "auto-saves/") t)

(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "backups/"))))
(make-directory (concat user-emacs-directory "backups/") t)

;; Typescript
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-hook 'typescript-mode-hook 'eglot-ensure)


  
  
(setq display-buffer-alist
      '(("*Python*"
         (display-buffer-reuse-window
          display-buffer-at-bottom)
         (window-height . 0.3))))
