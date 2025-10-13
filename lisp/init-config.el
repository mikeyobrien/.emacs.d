;;; init-config.el --- Configuration variables -*- lexical-binding: t -*-

;;; Commentary:
;; Customizable configuration variables

;;; Code:

(defgroup mov-config nil
  "Personal Emacs configuration."
  :group 'emacs)

(defcustom mov-enable-meow t
  "Enable Meow modal editing."
  :type 'boolean
  :group 'mov-config)

(defcustom mov-font-size 150
  "Default font size."
  :type 'integer
  :group 'mov-config)

(defcustom mov-font-size-large 190
  "Font size for large displays."
  :type 'integer
  :group 'mov-config)

(provide 'init-config)
;;; init-config.el ends here
