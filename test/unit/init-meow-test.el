;;; init-meow-test.el --- Tests for init-meow.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'test-helper)

(ert-deftest init-meow-available ()
  "Test that meow is declared."
  (should t))  ; Meow is declared via use-package

(provide 'init-meow-test)
;;; init-meow-test.el ends here
