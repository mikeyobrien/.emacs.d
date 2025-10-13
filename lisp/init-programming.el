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
  
  :init
  (add-hook 'flymake-mode-hook 'flymake-flycheck-auto)
  (add-hook 'prog-mode-hook 'flymake-mode)
  (add-hook 'text-mode-hook 'flymake-mode))

;; TypeScript
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)


;; JavaScript: prefer tree-sitter when available
(when (fboundp 'js-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.\\(js\\|mjs\\|cjs\\|es6\\)\\(\\.erb\\)?\\'" . js-ts-mode)))
;; Fallback (kept for older Emacs)
(add-to-list 'auto-mode-alist '("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js-mode))

;; Nix
(use-package nix-mode
  )

;; Markdown (modern setup)
(use-package markdown-mode
  
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-fontify-code-blocks-natively t
        markdown-hide-markup t
        markdown-enable-wiki-links t)
  :hook
  ((markdown-mode . gptel-mode)
   (gfm-mode . gptel-mode)
   (markdown-mode . visual-line-mode)
   (markdown-mode . variable-pitch-mode)
   (markdown-mode . (lambda () (display-line-numbers-mode -1))))
  :bind (:map markdown-mode-map
              ("C-c C-i" . markdown-insert-image)))

(use-package markdown-toc
  :after markdown-mode
  :bind (:map markdown-mode-map
              ("C-c m t" . markdown-toc-generate-toc)))

;; Optional: GitHub-flavored live preview if grip is installed
(use-package grip-mode
  :after markdown-mode
  :if (executable-find "grip")
  :bind (:map markdown-mode-map
              ("C-c m p" . grip-mode)))

;; Eglot LSP for Markdown via marksman (if installed)
(with-eval-after-load 'eglot
  (when (executable-find "marksman")
    (add-to-list 'eglot-server-programs '((gfm-mode markdown-mode) . ("marksman")))) )

;; Emacs Lisp

(use-package elisp-slime-nav
  
  :hook (emacs-lisp-mode . elisp-slime-nav-mode)
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "ELisp"))))



(provide 'init-programming)
;;; init-programming.el ends here
