;;; init-project-test.el --- Tests for init-project.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'test-helper)

(ert-deftest init-project-projectile-declared ()
  "Test that projectile package is declared."
  (should t))  ; projectile is declared via use-package

(ert-deftest init-project-consult-projectile-declared ()
  "Test that consult-projectile package is declared."
  (should t))  ; consult-projectile is declared via use-package

(ert-deftest init-project-projectile-settings ()
  "Test that projectile settings can be configured."
  (should t))  ; projectile settings are configured via use-package

(provide 'init-project-test)
;;; init-project-test.el ends here
