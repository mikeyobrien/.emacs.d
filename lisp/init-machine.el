;;; init-machine.el --- Machine-specific configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Detect machine type and load appropriate configuration

;;; Code:

(defvar mov-machine-type
  (cond
   ((string-equal system-type "android") 'android)
   ((let ((machine-config (expand-file-name "machine-config.el" user-emacs-directory)))
      (when (file-exists-p machine-config)
        (load machine-config)
        (bound-and-true-p is-work-machine)))
    'work)
   (t 'personal))
  "Type of machine: 'work, 'android, or 'personal.")

(defun mov-load-machine-config ()
  "Load machine-specific configuration."
  (pcase mov-machine-type
    ('work
     (require 'init-work)
     (message "Work configuration loaded"))
    ('android
     (require 'init-android)
     (message "Android configuration loaded"))))

(provide 'init-machine)
;;; init-machine.el ends here
