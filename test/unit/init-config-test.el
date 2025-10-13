;;; init-config-test.el --- Tests for init-config.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'test-helper)
(require 'init-config)

(ert-deftest init-config-group-defined ()
  "Test that mov-config group is defined."
  (should (get 'mov-config 'group-documentation)))

(ert-deftest init-config-meow-variable ()
  "Test that mov-enable-meow is defined."
  (should (boundp 'mov-enable-meow))
  (should (booleanp mov-enable-meow)))

(ert-deftest init-config-font-size ()
  "Test that font size variables are defined."
  (should (boundp 'mov-font-size))
  (should (numberp mov-font-size))
  (should (> mov-font-size 0)))

(ert-deftest init-config-font-size-large ()
  "Test that large font size is defined."
  (should (boundp 'mov-font-size-large))
  (should (numberp mov-font-size-large))
  (should (> mov-font-size-large mov-font-size)))

(provide 'init-config-test)
;;; init-config-test.el ends here
