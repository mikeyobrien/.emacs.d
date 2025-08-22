;;; init-completion.el --- Completion and navigation -*- lexical-binding: t -*-

;;; Commentary:
;; Vertico, Corfu, Embark, and completion configuration

;;; Code:

;; Vertico (vertical completion)
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t))

;; Marginalia (completion annotations)
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

;; Corfu (in-buffer completion)
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 250)
  (corfu-auto-prefix 250)
  :init
  (global-corfu-mode))

;; Completion settings
(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))

;; Orderless (flexible completion)
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (orderless))))))

;; Embark (contextual actions)
(use-package embark
  :bind
  (("C-.". embark-act)
   ("M-.". embark-dwim)
   ("C-,". embark-act-all)))

(provide 'init-completion)
;;; init-completion.el ends here
