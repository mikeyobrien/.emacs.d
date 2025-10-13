;;; early-init-test.el --- Tests for early-init.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'test-helper)

(ert-deftest early-init-package-disabled ()
  "Test that package.el is disabled."
  (should (eq package-enable-at-startup nil)))

(ert-deftest early-init-load-prefer-newer ()
  "Test that load-prefer-newer is set."
  (should (boundp 'load-prefer-newer)))

(ert-deftest early-init-gc-threshold ()
  "Test that GC threshold is configured."
  (should (numberp gc-cons-threshold))
  (should (> gc-cons-threshold 0)))

(ert-deftest early-init-gc-percentage ()
  "Test that GC percentage is configured."
  (should (numberp gc-cons-percentage))
  (should (> gc-cons-percentage 0)))

(provide 'early-init-test)
;;; early-init-test.el ends here
