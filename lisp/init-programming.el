;;; init-programming.el --- Programming languages -*- lexical-binding: t -*-

;;; Commentary:
;; Language-specific configurations and tree-sitter

;;; Code:

;; Tree-sitter
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package flymake
  :ensure nil)

(use-package flymake-flycheck
  :ensure t
  :init
  (add-hook 'flymake-mode-hook 'flymake-flycheck-auto)
  (add-hook 'prog-mode-hook 'flymake-mode)
  (add-hook 'text-mode-hook 'flymake-mode))

;; TypeScript
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-hook 'typescript-mode-hook 'eglot-ensure)


;; JavaScript
(add-to-list 'auto-mode-alist '("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js-mode))

;; Nix
(use-package nix-mode
  :ensure t)

;; Markdown
(use-package markdown-mode
  :ensure t
  :config
  (add-hook 'markdown-mode-hook #'gptel-mode))

;; Emacs Lisp

(use-package elisp-slime-nav
  :ensure t
  :hook (emacs-lisp-mode . elisp-slime-nav-mode)
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "ELisp"))))



(provide 'init-programming)
;;; init-programming.el ends here
