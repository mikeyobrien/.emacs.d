;;; ai-mcp-test.el --- Tests for ai/mcp.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'test-helper)

(ert-deftest ai-mcp-module-loadable ()
  "Test that MCP module exists."
  (should (file-exists-p (expand-file-name "lisp/ai/mcp.el" user-emacs-directory))))

(provide 'ai-mcp-test)
;;; ai-mcp-test.el ends here
