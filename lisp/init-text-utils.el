;;; init-text-utils.el --- Text manipulation utilities -*- lexical-binding: t -*-

;;; Commentary:
;; Utility functions for text editing

;;; Code:

(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(global-set-key (kbd "M-Q") 'unfill-paragraph)

(provide 'init-text-utils)
;;; init-text-utils.el ends here
