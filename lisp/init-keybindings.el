;;; init-keybindings.el --- Global key bindings -*- lexical-binding: t -*-

;;; Commentary:
;; Global key mappings and custom functions

;;; Code:


;; Macro for creating prefix commands with nested keybindings
(use-package general
  :init
  (unbind-key "C-c b")
  :config
  (general-create-definer mov/leader-keys
    :prefix "C-c")

  (mov/leader-keys
    "q"  '(:ignore t :which-key "emacs")
    "qq" '(save-buffers-kill-terminal :which-key "quit emacs")
    "qQ" '(restart-emacs :which-key "restart emacs")
    "qr" '(reload-emacs-config :which-key "reload config")

    "w"  '(:ignore t :which-key "window")
    "wp" '(winner-undo :which-key "undo")
    "wn" '(winner-redo :which-key "redo")
    "wo" '(ace-window :which-key "ace window")

    "f"  '(:ignore t :which-key "files")
    "ff" '(find-file :which-key "find file")
    "fe" '(consult-recent-file :which-key "recent files")
    
    "b"  '(:ignore t :which-key "buffers")
    "bb" '(consult-buffer :which-key "switch buffer")
    "bn" '(next-buffer :which-key "next buffer")
    "bp" '(previous-buffer :which-key "previous buffer")
    "bk" '(kill-buffer :which-key "kill buffer"))

  (general-define-key
   "M-o" 'ace-window
   "C-c C-/" 'consult-ripgrep))



;; Scroll hydra
(use-package hydra
  :ensure t)

(defhydra hydra-scroll (:color red :hint nil)
  "
^Scroll^
^------^
_d_: scroll down (C-d)
_u_: scroll up (C-u)
_q_: quit
"
  ("d" scroll-up-command)
  ("u" scroll-down-command)
  ("q" nil))

;; Window management hydra
(defhydra hydra-window (:color red :hint nil)
  "
^Move^          ^Split^         ^Resize^
^----^          ^-----^         ^------^
_h_: left       _v_: vertical   _H_: shrink horizontal
_j_: down       _s_: horizontal _J_: shrink vertical
_k_: up         _d_: delete     _K_: enlarge vertical
_l_: right      _o_: delete others _L_: enlarge horizontal
_q_: quit
"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("v" split-window-right)
  ("s" split-window-below)
  ("d" delete-window)
  ("o" delete-other-windows)
  ("H" shrink-window-horizontally)
  ("J" shrink-window)
  ("K" enlarge-window)
  ("L" enlarge-window-horizontally)
  ("q" nil))

;; Text scale hydra
(defhydra hydra-zoom (:color red :hint nil)
  "
^Zoom^
^----^
_+_: increase
_-_: decrease
_0_: reset
_q_: quit
"
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("0" (text-scale-adjust 0))
  ("q" nil))

;; Rectangle operations hydra
(defhydra hydra-rectangle (:color pink :hint nil)
  "
^Move^          ^Edit^
^----^          ^----^
_h_: left       _t_: type
_j_: down       _d_: kill
_k_: up         _y_: yank
_l_: right      _c_: clear
_q_: quit
"
  ("h" rectangle-backward-char)
  ("l" rectangle-forward-char)
  ("k" rectangle-previous-line)
  ("j" rectangle-next-line)
  ("t" string-rectangle)
  ("d" kill-rectangle)
  ("y" yank-rectangle)
  ("c" clear-rectangle)
  ("q" nil))

;; Keybindings
(global-set-key (kbd "C-c j") 'hydra-scroll/body)
(global-set-key (kbd "C-c C-w") 'hydra-window/body)
(global-set-key (kbd "C-c z") 'hydra-zoom/body)
(global-set-key (kbd "C-c r") 'hydra-rectangle/body)

(provide 'init-keybindings)

;;; init-keybindings.el ends here
