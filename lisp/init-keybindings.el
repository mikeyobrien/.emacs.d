;;; init-keybindings.el --- Global key bindings -*- lexical-binding: t -*-

;;; Commentary:
;; Global non-modal key mappings

;;; Code:

(require 'init-hydras)
(require 'init-text-utils)

(use-package restart-emacs)

;; Global hydra bindings
(global-set-key (kbd "C-c w s") 'hydra-scroll/body)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
