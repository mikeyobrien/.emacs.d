;;; ai-backends-test.el --- Tests for ai/backends.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'test-helper)

(ert-deftest ai-backends-module-loadable ()
  "Test that AI backends module exists."
  (should (file-exists-p (expand-file-name "lisp/ai/backends.el" user-emacs-directory))))

(provide 'ai-backends-test)
;;; ai-backends-test.el ends here
