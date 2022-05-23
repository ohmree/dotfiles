;;; lang/rest/config.el -*- lexical-binding: t; -*-

(use-package! verb
  :config
  (setq verb-enable-elisp-completion
        (featurep! :completion company)))
