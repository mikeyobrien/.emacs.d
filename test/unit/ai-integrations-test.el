;;; ai-integrations-test.el --- Tests for ai/integrations.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'test-helper)

(ert-deftest ai-integrations-module-loadable ()
  "Test that AI integrations module exists."
  (should (file-exists-p (expand-file-name "lisp/ai/integrations.el" user-emacs-directory))))

(provide 'ai-integrations-test)
;;; ai-integrations-test.el ends here
