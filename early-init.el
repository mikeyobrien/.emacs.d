;; -*- lexical-binding: t; -*-

;; Startup optimizations
(setq package-enable-at-startup nil)
(defvar mov--old-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist mov--old-file-name-handler-alist)
            (setq gc-cons-threshold (* 128 1024 1024)
                  gc-cons-percentage 0.1)))

;; Android (Termux) tweaks
(when (string-equal system-type "android")
  ;; Add Termux binaries to PATH environment
  (let ((termuxpath "/data/data/com.termux/files/usr/bin")
        (localpath "/data/data/com.termux/files/home/.local/bin"))
    (setenv "PATH" (concat (getenv "PATH") ":" termuxpath ":" localpath))
    (setq touch-screen-display-keyboard t)
    (add-to-list 'exec-path termuxpath)
    (add-to-list 'exec-path localpath)))
