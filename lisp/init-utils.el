;; -*- lexical-binding: t; -*-

(defun mov/hello ()
  (interactive)
  (message "hello, world!"))

(defun reload-emacs-config ()
  "Reload the Emacs configuration by clearing features and reloading init.el."
  (interactive)
  ;; Get all .el files in lisp/ directory and convert to feature names
  (let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
    (when (file-directory-p lisp-dir)
      (dolist (file (directory-files lisp-dir nil "\\.el$"))
        (let ((feature (intern (file-name-sans-extension file))))
          (when (featurep feature)
            (unload-feature feature t))))))
  ;; Reload the main init file
  (load-file user-init-file)
  (message "Emacs configuration reloaded"))

(provide 'init-utils)
