;;; ai-helpers-test.el --- Tests for ai/helpers.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'test-helper)

(ert-deftest ai-helpers-module-loadable ()
  "Test that AI helpers module exists."
  (should (file-exists-p (expand-file-name "lisp/ai/helpers.el" user-emacs-directory))))

(provide 'ai-helpers-test)
;;; ai-helpers-test.el ends here
