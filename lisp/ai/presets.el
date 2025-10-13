;;; presets.el --- AI helper functions -*- lexical-binding: t -*-

;;; Commentary:
;; Common AI tasks: explain, docstring, tests, refactor

;;; Code:



(require 'gptel)


(gptel-make-preset
  :name "gpt-with-fetch"
  :description "GPT model with fetch tools enabled"
  :config
  (lambda ()
    (setq gptel-model "gpt-4o-mini"
          gptel-tools '(fetch))))

