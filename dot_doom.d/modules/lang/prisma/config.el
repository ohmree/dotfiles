;;; lang/prisma/config.el -*- lexical-binding: t; -*-

(use-package! prisma-mode
  :config
  (when (featurep! +lsp)
    (add-hook 'prisma-mode-local-vars-hook #'lsp!)))
