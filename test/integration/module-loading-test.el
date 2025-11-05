;;; module-loading-test.el --- Module loading integration tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for module loading order and dependencies

;;; Code:

(require 'ert)
(require 'test-helper)

(ert-deftest integration-init-config-loads-first ()
  "Test that init-config.el can load independently."
  (should (require 'init-config nil t))
  (should (boundp 'mov-enable-meow))
  (should (boundp 'mov-font-size))
  (should (boundp 'mov-font-size-large)))

(ert-deftest integration-config-variables-have-correct-types ()
  "Test that configuration variables have expected types."
  (require 'init-config)
  (should (booleanp mov-enable-meow))
  (should (integerp mov-font-size))
  (should (integerp mov-font-size-large)))

(ert-deftest integration-init-utils-loads ()
  "Test that init-utils.el loads and provides expected functions."
  (require 'init-utils)
  (should (fboundp 'mov/hello))
  (should (fboundp 'reload-emacs-config)))

(ert-deftest integration-reload-config-function-exists ()
  "Test that reload-emacs-config function is callable."
  (require 'init-utils)
  (should (commandp 'reload-emacs-config)))

(ert-deftest integration-machine-config-detection ()
  "Test machine type detection logic."
  (require 'init-machine)
  (should (boundp 'mov-machine-type))
  (should (memq mov-machine-type '(work android personal))))

(ert-deftest integration-machine-config-loader-exists ()
  "Test that machine config loader function exists."
  (require 'init-machine)
  (should (fboundp 'mov-load-machine-config)))

(ert-deftest integration-ai-backends-graceful-degradation ()
  "Test that AI backends handle missing secrets gracefully."
  ;; Don't load secrets
  (setq openwebui-api-key nil)

  ;; Should load without crashing even if key is nil
  (should (require 'backends nil t))

  ;; Backend variables should be defined
  (should (boundp 'gptel-openwebui))
  (should (boundp 'gptel-bedrock)))

(ert-deftest integration-ai-helpers-functions-available ()
  "Test that AI helper functions are available."
  (require 'helpers)
  (should (fboundp 'mov/gptel-explain-region))
  (should (fboundp 'mov/gptel-docstring-region))
  (should (fboundp 'mov/gptel-tests-region))
  (should (fboundp 'mov/gptel-refactor-region))
  (should (fboundp 'mov/gptel-complete))
  (should (fboundp 'mov/gptel-insert)))

(ert-deftest integration-modules-load-order ()
  "Test that modules can be loaded in init.el order without errors."
  (let ((modules '(init-config
                   init-utils
                   init-machine)))
    (dolist (module modules)
      (should (require module nil t)))))

(provide 'module-loading-test)
;;; module-loading-test.el ends here
