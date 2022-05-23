(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(package-selected-packages
   '(oauth2-request ansi package-build shut-up epl git commander sly-quicklisp sly-asdf))
 '(safe-local-variable-values
   '((lsp-volar-take-over-mode)
     (eval lexical-let
           ((project-directory
             (car
              (dir-locals-find-file default-directory))))
           (eval-after-load 'lsp-javascript
             '(progn
                (plist-put lsp-deps-providers :local
                           (list :path
                                 (lambda
                                   (path)
                                   (concat project-directory ".yarn/sdks/" path))))))
           (lsp-dependency 'typescript-language-server
                           '(:local "typescript-language-server/lib/cli.js"))
           (lsp-dependency 'typescript
                           '(:local "typescript/bin/tsserver")))
     (eval lexical-let
           ((project-directory
             (car
              (dir-locals-find-file default-directory))))
           (eval-after-load 'lsp-mode
             '(progn
                (plist-put lsp-deps-providers :local
                           (list :path
                                 (lambda
                                   (path)
                                   (concat project-directory ".yarn/sdks/" path))))))
           (lsp-dependency 'typescript-language-server
                           '(:local "typescript-language-server/lib/cli.js"))
           (lsp-dependency 'typescript
                           '(:local "typescript/bin/tsserver")))
     (eval lexical-let
           ((project-directory
             (car
              (dir-locals-find-file default-directory))))
           (eval-after-load 'lsp
             '(progn
                (plist-put lsp-deps-providers :local
                           (list :path
                                 (lambda
                                   (path)
                                   (concat project-directory ".yarn/sdks/" path))))))
           (lsp-dependency 'typescript-language-server
                           '(:local "typescript-language-server/lib/cli.js"))
           (lsp-dependency 'typescript
                           '(:local "typescript/bin/tsserver")))
     (eval lexical-let
           ((project-directory
             (car
              (dir-locals-find-file default-directory))))
           (eval-after-load 'lsp-clients
             '(progn
                (plist-put lsp-deps-providers :local
                           (list :path
                                 (lambda
                                   (path)
                                   (concat project-directory ".yarn/sdks/" path))))))
           (lsp-dependency 'typescript-language-server
                           '(:local "typescript-language-server/lib/cli.js"))
           (lsp-dependency 'typescript
                           '(:local "typescript/bin/tsserver")))
     (lsp-enabled-clients ts-ls)
     (geiser-scheme-implementation . mit)
     (geiser-default-implementation . mit)
     (geiser-active-implementations mit)
     (geiser-default-implementation quote mit)
     (geiser-scheme-implementation quote mit)
     (lsp-elixir-project-dir . "/home/ohmree/code/elixir/frick_dmca")
     (lsp-elixir-project-dir . "code/elixir/frick_dmca")
     (lsp-elixir-project-dir . "code/elixir/rumbl")
     (bidi-paragraph-direction quote right-to-left)
     (eval setq inferior-lisp-program
           (concat
            (projectile-project-root)
            "/run-repl"))))
 '(sql-connection-alist
   '(("Default"
      (sql-product 'postgres)
      (sql-user "ohmree")
      (sql-database "learning")
      (sql-server "localhost"))
     ("FrickDMCA"
      (sql-product 'postgres)
      (sql-user "ohmree")
      (sql-database "frick_dmca")
      (sql-server "localhost"))
     ("frick-dmca"
      (sql-product 'postgres)
      (sql-user "ohmree")
      (sql-database "frick-dmca")
      (sql-server "localhost"))))
 '(warning-suppress-types '((lsp-on-idle-hook) (iedit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:foreground "white" :background "red" :weight bold :height 2.5 :box (:line-width 10 :color "red"))))))
