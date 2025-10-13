;;; init-theme.el --- Theme configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Color themes and visual appearance

;;; Code:

(use-package doom-themes
  :demand t
  :config
  (load-theme 'doom-one t))

(use-package all-the-icons)

(use-package nerd-icons)

(provide 'init-theme)
;;; init-theme.el ends here
