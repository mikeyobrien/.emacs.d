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

(provide 'init-programming)
;;; init-programming.el ends here
