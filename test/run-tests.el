;;; run-tests.el --- Test runner -*- lexical-binding: t; -*-

;;; Commentary:
;; Run all ERT tests for Emacs configuration

;;; Code:

(require 'ert)

;; Load test helper
(load (expand-file-name "test-helper.el" (file-name-directory load-file-name)))

;; Load all test files
(dolist (test-file (directory-files-recursively
                    (file-name-directory load-file-name)
                    ".*-test\\.el$"))
  (load test-file nil t))

;; Run tests
(ert-run-tests-batch-and-exit)

;;; run-tests.el ends here
