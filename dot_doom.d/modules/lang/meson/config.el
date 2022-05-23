;;; lang/meson/config.el -*- lexical-binding: t; -*-

(use-package! meson-mode
  :config
  (add-hook 'meson-mode-hook 'company-mode)
  (when (featurep! :tools lookup)
    (set-lookup-handlers! 'meson-mode
      :documentation #'meson-lookup-doc)))
