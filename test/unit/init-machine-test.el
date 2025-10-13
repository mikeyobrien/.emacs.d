;;; init-machine-test.el --- Tests for init-machine.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'test-helper)
(require 'init-machine)

(ert-deftest init-machine-type-defined ()
  "Test that mov-machine-type is defined."
  (should (boundp 'mov-machine-type))
  (should (memq mov-machine-type '(work android personal))))

(ert-deftest init-machine-load-function ()
  "Test that mov-load-machine-config function exists."
  (should (fboundp 'mov-load-machine-config)))

(ert-deftest init-machine-type-valid ()
  "Test that machine type is one of expected values."
  (should (or (eq mov-machine-type 'work)
              (eq mov-machine-type 'android)
              (eq mov-machine-type 'personal))))

(provide 'init-machine-test)
;;; init-machine-test.el ends here
