;;; init-modeline.el --- Modeline configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Doom modeline setup

;;; Code:

(use-package doom-modeline
  :demand t
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 18 :weight 'regular))
  (nerd-icons-font-family "JetBrainsMono Nerd Font")
  (doom-symbol-font (font-spec :family "JetBrainsMono Nerd Font" :size 11)))

(provide 'init-modeline)
;;; init-modeline.el ends here
