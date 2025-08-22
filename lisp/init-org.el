;;; init-org.el --- Org mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Org mode configuration with sane defaults

;;; Code:

(use-package org
  :ensure nil
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link))
  :custom
  ;; Set org directory relative to default-directory
  (org-directory (expand-file-name "org" default-directory))
  
  ;; Basic org settings
  (org-startup-indented t)
  (org-startup-folded 'content)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis " ▼")
  
  ;; Agenda files
  (org-agenda-files (list org-directory))
  
  ;; Capture templates
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  
  ;; Todo keywords
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))
  
  ;; Log completion time
  (org-log-done 'time)
  (org-log-into-drawer t)
  
  ;; Refile targets
  (org-refile-targets '((nil :maxlevel . 3)
                       (org-agenda-files :maxlevel . 3)))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  
  ;; Source code blocks
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-edit-src-content-indentation 0)
  
  ;; Export settings
  (org-export-with-toc t)
  (org-export-with-section-numbers nil)
  (org-export-with-timestamps nil)
  
  ;; Appearance
  (org-hide-leading-stars t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  
  :config
  ;; Create org directory if it doesn't exist
  (unless (file-exists-p org-directory)
    (make-directory org-directory t))
  
  ;; Basic capture templates
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("n" "Note" entry (file+headline org-default-notes-file "Notes")
           "* %? :NOTE:\n%U\n%a\n")
          ("j" "Journal" entry (file+datetree (expand-file-name "journal.org" org-directory))
           "* %?\n%U\n"))))

;; Org-modern for better visuals (optional)
(use-package org-modern
  :after org
  :config
  (global-org-modern-mode))

(provide 'init-org)
;;; init-org.el ends here
