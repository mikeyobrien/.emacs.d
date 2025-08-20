;; -*- lexical-binding: t; -*-
(require 'package)



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

(use-package disable-mouse
  :init
  (global-disable-mouse-mode))

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
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
