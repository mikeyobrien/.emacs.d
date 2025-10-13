;;; helpers.el --- AI helper functions -*- lexical-binding: t -*-

;;; Commentary:
;; Common AI tasks: explain, docstring, tests, refactor

;;; Code:

(require 'gptel)

(defun mov/gptel--region-or-buffer-string ()
  "Get selected region or entire buffer content."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun mov/gptel-explain-region ()
  "Explain the selected code/text or current buffer."
  (interactive)
  (let ((content (mov/gptel--region-or-buffer-string)))
    (gptel-request
     (format "Explain the following content clearly and concisely.\n\n%s" content)
     :system "You are a senior engineer. Provide a precise, succinct explanation, include gotchas and edge cases when relevant."
     :callback (lambda (response _info)
                 (when response
                   (message "%s" response))))))

(defun mov/gptel-docstring-region ()
  "Write docstrings/comments for the selected code or buffer."
  (interactive)
  (let ((content (mov/gptel--region-or-buffer-string)))
    (gptel-request
     (format "Write idiomatic docstrings/comments for this code without changing behavior.\n\n%s" content)
     :system "Return only the improved code or docstrings as appropriate."
     :buffer (get-buffer-create "*gptel-docstring*"))))

(defun mov/gptel-tests-region ()
  "Generate focused tests for the selected code or buffer."
  (interactive)
  (let ((content (mov/gptel--region-or-buffer-string)))
    (gptel-request
     (format "Write unit tests for this code. Include edge cases and name tests descriptively.\n\n%s" content)
     :system "Prefer existing project test frameworks and helpers. Return only test code."
     :buffer (get-buffer-create "*gptel-tests*"))))

(defun mov/gptel-refactor-region ()
  "Refactor the selected code or buffer without changing behavior."
  (interactive)
  (let ((content (mov/gptel--region-or-buffer-string)))
    (gptel-request
     (format "Refactor to improve readability without altering behavior.\n\n%s" content)
     :system "Keep changes minimal and idiomatic. Return only code."
     :buffer (get-buffer-create "*gptel-refactor*"))))

(defun mov/gptel-complete (prompt)
  "Send PROMPT to GPTel and insert response at point."
  (interactive (list (read-string "Complete with AI: ")))
  (when (string-empty-p prompt)
    (user-error "A prompt is required"))
  (gptel-request
   prompt
   :callback
   (lambda (response _info)
     (when (and response (stringp response))
       (save-excursion
         (insert response))))))

(defun mov/gptel-insert ()
  "Insert AI completion at point."
  (interactive)
  (set-mark (point))
  (insert "\n")
  (exchange-point-and-mark)
  (gptel-rewrite))

(provide 'helpers)
;;; helpers.el ends here
