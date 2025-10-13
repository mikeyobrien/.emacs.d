;;; mcp.el --- Model Context Protocol configuration -*- lexical-binding: t -*-

;;; Commentary:
;; MCP server setup and configuration

;;; Code:

(use-package mcp
  :after gptel
  :demand t
  :config
  (require 'mcp)
  (require 'mcp-hub)
  (let* ((filesystem-root "/Users/mobrienv/")
         (servers (list (cons "fetch" '(:command "uvx" :args ("mcp-server-fetch")))
                        (cons "builder-mcp" '(:command "builder-mcp"))
                        (cons "filesystem"
                              `(:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" ,filesystem-root))))))
    (setq mcp-hub-servers servers))
  (run-with-timer 2 nil
                  (lambda ()
                    (condition-case err
                        (mcp-hub-start-all-server
                         (lambda ()
                           (message "MCP: All servers started successfully")))
                      (error (message "MCP: Failed to start servers: %S" err))))))

(provide 'mcp)
;;; mcp.el ends here
