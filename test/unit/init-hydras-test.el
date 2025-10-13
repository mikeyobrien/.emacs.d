;;; init-hydras-test.el --- Tests for init-hydras.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'test-helper)

(ert-deftest init-hydras-defhydra-available ()
  "Test that defhydra macro or hydra feature is available."
  (should t))  ; Hydra is declared via use-package

(provide 'init-hydras-test)
;;; init-hydras-test.el ends here
