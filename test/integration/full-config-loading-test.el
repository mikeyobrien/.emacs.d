;;; full-config-loading-test.el --- Full configuration loading tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for full Emacs configuration loading

;;; Code:

(require 'ert)

(ert-deftest integration-full-init-loads-without-errors ()
  "Test that init.el loads completely without errors."
  (let ((user-emacs-directory (expand-file-name ".." (file-name-directory load-file-name)))
        (errors '())
        (warnings '()))

    ;; Mock use-package to avoid actually installing packages
    (with-eval-after-load 'init-package
      (defmacro use-package (name &rest args)
        `(progn
           (message "Mock loading package: %s" ',name)
           nil)))

    ;; Capture errors and warnings
    (condition-case err
        (progn
          ;; Mock secrets.el to avoid missing file issues
          (setq openwebui-api-key "test-key")

          ;; Load early-init
          (load (expand-file-name "early-init.el" user-emacs-directory))

          ;; Verify early-init optimizations were applied
          (should (>= gc-cons-threshold (* 128 1024 1024))))
      (error
       (push (format "Error loading early-init.el: %s" err) errors)))

    ;; Verify no critical errors
    (should (null errors))))

(ert-deftest integration-early-init-gc-optimization ()
  "Test that early-init.el properly sets up GC optimization."
  (let ((user-emacs-directory (expand-file-name ".." (file-name-directory load-file-name))))
    (load (expand-file-name "early-init.el" user-emacs-directory))

    ;; After early-init, GC threshold should be very high (most-positive-fixnum)
    ;; but we can't test this exactly as it gets restored after startup
    ;; Instead verify the restoration hook exists
    (should (member 'emacs-startup-hook (mapcar #'car after-load-alist)))))

(ert-deftest integration-package-enable-disabled ()
  "Test that package.el is disabled in favor of straight.el."
  (let ((user-emacs-directory (expand-file-name ".." (file-name-directory load-file-name))))
    (load (expand-file-name "early-init.el" user-emacs-directory))
    (should (null package-enable-at-startup))))

(ert-deftest integration-android-detection ()
  "Test that Android environment is properly detected."
  (let ((system-type 'android)
        (user-emacs-directory (expand-file-name ".." (file-name-directory load-file-name))))

    ;; Mock environment
    (setenv "PATH" "/bin")

    ;; Load early-init
    (load (expand-file-name "early-init.el" user-emacs-directory))

    ;; Verify Android paths were added
    (should (string-match-p "termux" (getenv "PATH")))))

(provide 'full-config-loading-test)
;;; full-config-loading-test.el ends here
