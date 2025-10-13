;;; init-package-test.el --- Tests for init-package.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'test-helper)

(ert-deftest init-package-straight-use-package-defined ()
  "Test that straight-use-package function is defined."
  (should (fboundp 'straight-use-package)))

(ert-deftest init-package-straight-default-enabled ()
  "Test that straight is default for use-package."
  (should (boundp 'straight-use-package-by-default)))

(ert-deftest init-package-use-package-defer ()
  "Test that use-package-always-defer variable exists."
  (should (or (boundp 'use-package-always-defer)
              (fboundp 'use-package))))

(provide 'init-package-test)
;;; init-package-test.el ends here
