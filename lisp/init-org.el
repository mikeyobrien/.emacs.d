;;; init-org.el --- Org mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Org mode configuration with sane defaults

;;; Code:

(defun mov/open-org-notes ()
  "Open the notes.org file."
  (interactive)
  (find-file "~/org/notes.org"))

(defun mov/search-org ()
  "Set working directory to ~/org then run consult-ripgrep"
  (interactive)
  (let ((default-directory (expand-file-name "~/org")))
    (consult-ripgrep)))


(use-package org
  :ensure nil
  :hook
  (org-mode . visual-line-mode)
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c C-l" . org-store-link))
  :custom
  ;; Set org directory relative to default-directory
  (org-directory "~/org")
  
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
  (org-refile-use-cache t)
  (org-refile-allow-creating-parent-nodes 'confirm)
  
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
  
  ;; Links & IDs
  (org-id-link-to-org-use-id 'create-if-interactive)

    
  :bind (("C-c n s" . mov/search-org)
	 ("C-c n n" . mov/open-org-notes))
  :config
  ;; Enable tempo for <s TAB expansion
  (require 'org-tempo)
  ;; Protocols & IDs
  (require 'org-protocol)
  (require 'org-id)
  ;; Habits in agenda
  (require 'org-habit)
  
  ;; Create org directory if it doesn't exist
  (unless (file-exists-p org-directory)
    (make-directory org-directory t))
  
  ;; Basic capture templates
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("i" "Inbox" entry (file "~/org/inbox.org")
           "* TODO %?\n:CREATED: %U\n%a\n")
          ("l" "Link" entry (file+headline "~/org/notes.org" "Bookmarks")
           "* %? [[%:link][%:description]]\n:CREATED: %U\n" :immediate-finish t :empty-lines 1)
          ("n" "Note" entry (file+headline org-default-notes-file "Notes")
           "* %? :NOTE:\n%U\n%a\n")
          ("j" "Work Log" entry (file+datetree "~/org/journal.org")
           "* %?" :empty-lines 0)))

  ;; Prevent org-capture from opening in new frame
  (setq pop-up-frames nil)
  (add-to-list 'display-buffer-alist
               '("\\*Capture\\*" display-buffer-same-window))

  ;; Enable word wrap in capture buffers
  (add-hook 'org-capture-mode-hook 'visual-line-mode)

  ;; Custom agenda: quick overview
  (setq org-agenda-custom-commands
        '(("o" "Overview"
           ((agenda "" ((org-agenda-span 'day)))
            (todo "NEXT")
            (todo "WAIT")))))

  ;; Enable common babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell       . t)
     (python      . t))))


;; Org-roam for networked thought
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename (expand-file-name "roam" org-directory)))

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory t))
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(provide 'init-org)
;;; init-org.el ends here
