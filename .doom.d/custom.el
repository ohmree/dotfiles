(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(sly-quicklisp sly-asdf))
 '(safe-local-variable-values
   '((eval setq inferior-lisp-program
           (concat
            (projectile-project-root)
            "/run-repl"))))
 '(sql-connection-alist
   '(("Default"
      (sql-product 'postgres)
      (sql-user "ohmree")
      (sql-database "learning")
      (sql-server "localhost")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
