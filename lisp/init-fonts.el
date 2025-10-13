;;; init-fonts.el --- Font configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Font setup with fallbacks

;;; Code:

(require 'init-config)

(defun setup-fonts ()
  "Set up fonts, preferring IosevkaTerm Nerd Font if available."
  (let ((font-height (if (string-equal (system-name) "g14") 
                         mov-font-size-large 
                       mov-font-size)))
    (cond
     ((find-font (font-spec :name "IosevkaTerm Nerd Font"))
      (set-face-attribute 'default nil
                          :font "IosevkaTerm Nerd Font"
                          :height font-height)
      (set-fontset-font t 'symbol "IosevkaTerm Nerd Font" nil 'prepend)
      (message "Using IosevkaTerm Nerd Font"))
     ((find-font (font-spec :name "FiraCode Nerd Font"))
      (set-face-attribute 'default nil
                          :font "FiraCode Nerd Font"
                          :height font-height)
      (set-fontset-font t 'symbol "FiraCode Nerd Font" nil 'prepend)
      (message "Using FiraCode Nerd Font"))
     ((find-font (font-spec :name "Fira Code"))
      (set-face-attribute 'default nil
                          :font "Fira Code"
                          :height font-height)
      (message "Using Fira Code"))
     (t
      (set-face-attribute 'default nil :height font-height)
      (message "IosevkaTerm/Fira Code fonts not found, using default font")))))

(setup-fonts)

;; Enable ligatures if available
(when (and (fboundp 'mac-auto-operator-composition-mode)
           (or (member "IosevkaTerm Nerd Font" (font-family-list))
               (member "FiraCode Nerd Font" (font-family-list))))
  (mac-auto-operator-composition-mode t))

(provide 'init-fonts)
;;; init-fonts.el ends here
