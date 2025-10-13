;;; test-helper.el --- Helper for tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

;; Add lisp directory to load path
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/ai" (file-name-directory load-file-name)))

;; Suppress package initialization during tests
(setq package-enable-at-startup nil)

;; Minimal straight.el mock for testing
(defvar straight-use-package-by-default nil)
(defun straight-use-package (&rest _args) t)

(provide 'test-helper)
;;; test-helper.el ends here
