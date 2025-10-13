;;; keybindings.el --- AI keybindings and transient menu -*- lexical-binding: t -*-

;;; Commentary:
;; All AI-related keybindings and transient menu

;;; Code:

(require 'gptel)
(require 'transient)
(require 'backends)
(require 'helpers)
(require 'integrations)

;; AI prefix map
(define-prefix-command 'ai-map)
(global-set-key (kbd "C-c l") 'ai-map)

;; Direct keybindings
(define-key ai-map (kbd "l") 'gptel)
(define-key ai-map (kbd "r") 'gptel-rewrite)
(define-key ai-map (kbd "a") 'gptel-lookup)
(define-key ai-map (kbd "m") 'gptel-menu)
(define-key ai-map (kbd "s") 'gptel-switch-backend)
(define-key ai-map (kbd "o") 'gptel-switch-to-openwebui)
(define-key ai-map (kbd "b") 'gptel-switch-to-bedrock)
(define-key ai-map (kbd "?") 'gptel-quick)
(define-key ai-map (kbd "E") 'mov/gptel-explain-region)
(define-key ai-map (kbd "D") 'mov/gptel-docstring-region)
(define-key ai-map (kbd "T") 'mov/gptel-tests-region)
(define-key ai-map (kbd "R") 'mov/gptel-refactor-region)
(define-key ai-map (kbd "C") 'mov/gptel-complete)
(define-key ai-map (kbd "A") 'mov/gptel-add-files-from-dired)
(define-key ai-map (kbd "S") 'mov/gptel-eww-summarize)
(define-key ai-map (kbd "p") 'mov/gptel-apply-project-system-prompt)
(define-key ai-map (kbd "L") 'mov/gptel-write-dir-locals-example)

;; Transient menu
(transient-define-prefix mov/gptel-transient ()
  "AI actions menu."
  ["Chats"
   ("l" "Chat"           gptel)
   ("r" "Rewrite"        gptel-rewrite)
   ("a" "Lookup"         gptel-lookup)
   ("m" "GPTel menu"     gptel-menu)]
  ["Helpers"
   ("E" "Explain"        mov/gptel-explain-region)
   ("D" "Docstrings"     mov/gptel-docstring-region)
   ("T" "Tests"          mov/gptel-tests-region)
   ("R" "Refactor"       mov/gptel-refactor-region)]
  ["Backend/Model"
   ("s" "Switch backend" gptel-switch-backend)
   ("o" "OpenWebUI"      gptel-switch-to-openwebui)
   ("b" "Bedrock"        gptel-switch-to-bedrock)]
  ["Context & Files"
   ("A" "Attach from Dired" mov/gptel-add-files-from-dired)
   ("S" "Summarize EWW page" mov/gptel-eww-summarize)
   ("p" "Apply project prompt" mov/gptel-apply-project-system-prompt)
   ("L" "Write dir-locals example" mov/gptel-write-dir-locals-example)])

(define-key ai-map (kbd ".") 'mov/gptel-transient)

(provide 'keybindings)
;;; keybindings.el ends here
