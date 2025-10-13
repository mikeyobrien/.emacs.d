;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Early initialization - runs before init.el

;;; Code:

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Prefer newer source files
(setq load-prefer-newer t)

;; Startup optimizations
(defvar mov--old-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Restore after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist mov--old-file-name-handler-alist)
            (setq gc-cons-threshold (* 128 1024 1024)
                  gc-cons-percentage 0.1)))

;; Android (Termux) tweaks
(when (string-equal system-type "android")
  (let ((termuxpath "/data/data/com.termux/files/usr/bin")
        (localpath "/data/data/com.termux/files/home/.local/bin"))
    (setenv "PATH" (concat (getenv "PATH") ":" termuxpath ":" localpath))
    (setq touch-screen-display-keyboard t)
    (add-to-list 'exec-path termuxpath)
    (add-to-list 'exec-path localpath)))

;;; early-init.el ends here
