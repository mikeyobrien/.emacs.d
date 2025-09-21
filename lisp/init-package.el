;;; init-package.el --- Package management setup -*- lexical-binding: t -*-

;;; Commentary:
;; Package management and use-package configuration

;;; Code:

(require 'package)
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
;; Defer packages by default; explicitly demand essentials
(setq use-package-always-defer t)

(provide 'init-package)
;;; init-package.el ends here
