;;; lang/asm/config.el -*- lexical-binding: t; -*-

(use-package! lsp-asm
  :when (featurep! +lsp)
  :hook (asm-mode . lsp!))
