;;; init-hydras.el --- Hydra definitions -*- lexical-binding: t -*-

;;; Commentary:
;; All hydra menu definitions

;;; Code:

(use-package hydra)

;; Scroll hydra
(defhydra hydra-scroll (:color red :hint nil)
  "
^Scroll^
^------^
_k_: scroll down (C-d in vim)
_j_: scroll up (C-u in vim)
_q_: quit
"
  ("j" (lambda () (interactive) (scroll-up-command (/ (window-body-height) 2))))
  ("k" (lambda () (interactive) (scroll-down-command (/ (window-body-height) 2))))
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

(provide 'init-hydras)
;;; init-hydras.el ends here
