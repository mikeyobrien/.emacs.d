;;; init-keybindings-test.el --- Tests for init-keybindings.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'test-helper)

(ert-deftest init-keybindings-restart-emacs ()
  "Test that restart-emacs is available."
  (should (or (fboundp 'restart-emacs)
              (featurep 'restart-emacs))))

(ert-deftest init-keybindings-global-set-key ()
  "Test that global-set-key function exists."
  (should (fboundp 'global-set-key)))

(provide 'init-keybindings-test)
;;; init-keybindings-test.el ends here
