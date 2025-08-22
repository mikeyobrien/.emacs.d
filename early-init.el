;; -*- lexical-binding: t; -*-
(when (string-equal system-type "android")
  ;; Add Termux binaries to PATH environment
  (let ((termuxpath "/data/data/com.termux/files/usr/bin")
        (localpath "/data/data/com.termux/files/home/.local/bin"))
    (setenv "PATH" (concat (getenv "PATH") ":" termuxpath ":" localpath))
    (setq touch-screen-display-keyboard t)
    (add-to-list 'exec-path termuxpath)
    (add-to-list 'exec-path localpath)))

  
