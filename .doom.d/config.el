;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(use-package! elcord)

(keychain-refresh-environment)

;; TODO: fix this
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq! user-full-name "me :)")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq! doom-font (font-spec :family "Fira Code" :size 16 :weight 'normal)
       doom-variable-pitch-font (font-spec :family "sans" :size 16 :weight 'normal)
       doom-unicode-font (font-spec :family "Noto Color Emoji"))

;; (set-fontset-font t 'hebrew "Noto Sans Hebrew")
;; (set-fontset-font t 'hebrew (font-spec :script 'hebrew) nil 'append)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq! doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq! org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq! display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;; (after! lispy
;;   (add-to-list 'lispyville-key-theme 'text-objects)
;;   (add-to-list 'lispyville-key-theme 'additional-motions)
;;   (add-to-list 'lispyville-key-theme 'commentary)
;;   (add-to-list 'lispyville-key-theme 'wrap)
;;   (add-to-list 'lispyville-key-theme 'escape))

(put 'with 'lisp-indent-function 1)

;; (defun sly-qlot-exec (directory)
;;   (interactive (list (read-directory-name "Project directory: ")))
;;   (sly-start :program "qlot"
;;              :program-args '("exec" "--with-client" "ros" "-S" "." "run")
;;              :directory directory
;;              :name 'qlot
;;              :env (list (concat "PATH="
;;                                 (mapconcat 'identity exec-path ":"))
;;                         (concat "QUICKLISP_HOME="
;;                                 (file-name-as-directory directory) "quicklisp/"))))

;; (defun sly-qlot-exec (directory)
;;   (interactive (list (read-directory-name "Project directory: ")))
;;   (sly-start :program "qlot"
;;              :program-args '("exec" "--with-client" "ros" "-S" "." "run")
;;              :directory directory
;;              :name 'qlot
;;              :env (list (concat "PATH=" (mapconcat 'identity exec-path ":")))))

;; Spacemacs-like auto-paren skipping
;; (defun smart-closing-parenthesis ()
;;   (interactive)
;;   (let* ((sp-navigate-close-if-unbalanced t)
;;          (current-pos (point))
;;          (current-line (line-number-at-pos current-pos))
;;          (next-pos (save-excursion
;;                      (sp-up-sexp)
;;                      (point)))
;;          (next-line (line-number-at-pos next-pos)))
;;     (cond
;;      ((and (= current-line next-line)
;;            (not (= current-pos next-pos)))
;;       (sp-up-sexp))
;;      (t
;;       (insert-char ?\))))))
;; (after! rustic
;;   (map! :i
;;         :mode 'rustic-mode
;;         ")" #'smart-closing-parenthesis))
;; (define-key! evil-insert-state-map ")" 'smart-closing-parenthesis)

;; (add-to-list 'company-backends #'company-tabnine)
;; Default value: company-capf
;; (setq! +lsp-company-backends '(company-tabnine company-capf))

(after! sly
  (map!
   :mode 'sly-mode
   (:prefix "]"
    :no "e" #'next-error)
   (:prefix "["
    :no "e" #'previous-error))
  (setq-default sly-lisp-implementations
                `((cmu ("clpm" "bundle" "exec" "--with-client" "cmucl"))
                  (sbcl ("clpm" "bundle" "exec" "--with-client" "sbcl"))
                  (abcl ("clpm" "bundle" "exec" "--with-client" "abcl"))
                  (ccl ("clpm" "bundle" "exec" "--with-client" "ccl"))
                  (ecl ("clpm" "bundle" "exec" "--with-client" "ecl"))
                  ;; (eql5 ("clpm" "bundle" "exec" "--with-client" ,(expand-file-name (concat doom-private-dir "eql5-slime.sh"))))
                  ;; (eql5 ("eql5"))
                  ))
  (setq-default sly-default-lisp 'sbcl))


(after! company
  ;; (setq! company-idle-delay 0)
  (setq! company-show-numbers t))

;; (setq! lsp-csharp-server-path "/opt/omnisharp-roslyn-stdio/OmniSharp.exe")

;; (setq! comp-deferred-compilation t)

;; We need this fix for some reason
(unless (fboundp 'cc-bytecomp-is-compiling)
  (defsubst cc-bytecomp-is-compiling ()
    "Return non-nil if eval'ed during compilation."
    (eq (cc-bytecomp-compiling-or-loading) 'compiling)))

(setq-default fill-column 120)

(setq +lookup-open-url-fn #'eww)

(after! dash-docs
  (setq dash-docs-docsets-path "~/.local/share/Zeal/Zeal/docsets/"
        dash-docs-browser-func #'eww))

;; (require 'dap-gdb-lldb)
;; (require 'dap-lldb)
;; (setq! dap-lldb-debug-program '("/usr/bin/lldb-vscode"))
;; (setq! dap-lldb-debugged-program-function (lambda () (concatenate 'string (projectile-project-root) "target/debug/" (projectile-project-name))))

(after! magit
  (setq! magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))

;; (after! eglot (add-to-list 'eglot-server-programs '((js-mode typescript-mode) "typescript-language-server" "--stdio")))
;; (setq-hook! 'rustic-mode-hook company-idle-delay nil)

;; (after! flycheck
;; (require 'flycheck-xo)
;;   (flycheck-xo-setup))
(after! lsp-mode
  ;; (setq! lsp-enable-file-watchers nil)

  (add-hook! 'hack-local-variables-hook
    (when (derived-mode-p 'typescript-mode) (lsp)))
  (setq! lsp-elixir-server-command '("/usr/bin/elixir-ls")))

;; (require 'lsp-toml)

(use-package! lsp-toml
  :config (setq! lsp-toml-schema-links t)
  :after-call conf-toml-mode-hook)

(after! lsp-css
  (add-to-list 'lsp-css-experimental-custom-data (concat doom-private-dir "postcss.css-data.json")))

(after! lsp-rust
  (setq! lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq! lsp-rust-analyzer-experimental-proc-attr-macros t)
  (setq! lsp-rust-analyzer-proc-macro-enable t)
  (setq! lsp-rust-target-dir "/tmp/lsp-rust-target"))

(after! magic
  (add-hook! magit-mode-hook #'(lambda () (magit-delta-mode +1))))

(after! counsel-dash
  (setq! counsel-dash-docsets-path "~/.local/share/Zeal/Zeal/docsets"))

(setq! +lookup-open-url-fn #'+lookup-xwidget-webkit-open-url-fn)

;; (setq! lsp-clients-lua-language-server-command '("/usr/bin/lua-language-server" ""))
;; (setq! lsp-clients-lua-language-server-install-dir "/usr/share/lua-language-server/")
;; (setq! lsp-clients-lua-language-server-bin "/usr/lib/lua-language-server/lua-language-server")
;; (setq! lsp-clients-lua-languag
;; e-server-args nil)
;; (setq! lsp-clients-lua-language-server-main-location nil)

;; (after! ccls
;;   (setq! ccls-initialization-options '(:index (:comments 2 :onChange t :trackDependency 2) :completion (:detailedLabel t)))
;;   (set-lsp-priority! 'ccls 2)) ; optional as ccls is the default in Doom

(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"
                                "--header-insertion-decorators=0"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))
(after! lsp-lua
  (setq! lsp-clients-lua-language-server-install-dir "/usr/lib/lua-language-server"
         lsp-clients-lua-language-server-bin "/usr/bin/lua-language-server"
         lsp-clients-lua-language-server-main-location "/usr/lib/lua-language-server/main.lua"))

(custom-set-faces!
  '(aw-leading-char-face
    :foreground "white" :background "red"
    :weight bold :height 2.5 :box (:line-width 10 :color "red")))

(defun load-environment-secrets-from-file (file)
  "Load secrets from a shell profile file in the format `export MY_SECRET=123abc...` into the current environment"
  (let* ((context (epg-make-context 'OpenPGP))
         (file-contents
          (decode-coding-string
           (epg-decrypt-file context file nil)
           'utf-8))
         (declarations (split-string file-contents "\n" t)))
    (dolist (declaration declarations)
      (when (string-match "export \\([_[:alnum:]]+\\)=\\(.+\\)" declaration)
        (setenv (match-string 1 declaration) (match-string 2 declaration))))))

;; (dolist (file (file-expand-wildcards (concat (getenv "HOME") "/.secrets/*")))
;;   (load-environment-secrets-from-file file))

(setq! fuel-factor-root-dir "/usr/lib/factor")
(elcord-mode)

(defun my-elcord-buffer-details-format ()
  "Return the buffer details string shown on discord, with a few tweaks."
  (pcase (buffer-name)
    ("*ielm*" "Inside IELM")
    ("*info*" "Reading info")
    ((rx "*Man" whitespace
         (let section (one-or-more not-newline)) whitespace
         (let name (one-or-more not-newline)) ?*)
     (format "Reading manpage for %s(%s)" name section))
    ((rx "*doom:vterm-popup:"
         (let name (one-or-more not-newline))
         ?*)
     (format "Inside terminal: %s" name))
    ((rx "*helpful "
         (let subject (one-or-more not-newline))
         ": "
         (let name (one-or-more not-newline))
         ?*)
     (format "Reading help for a %s `%s'" subject name))
    (_ (elcord-buffer-details-format))))

(setq! elcord-buffer-details-format-function #'my-elcord-buffer-details-format)
(add-to-list 'elcord-boring-buffers-regexp-list "\\*doom\\*")

(setq-default enable-local-variables t)

(after! org
  (setq! org-use-property-inheritance t)
  (setq! org-latex-compiler "lualatex")
  ; (use-package! ox-moderncv
  ;   :init (require 'ox-moderncv))
  )

(after! web-mode
  (define-derived-mode marko-mode web-mode "marko")
  (add-to-list 'auto-mode-alist '("\\.marko\\'" . marko-mode))
  (after! lsp-mode
    (add-to-list 'lsp-language-id-configuration '(marko-mode . "marko"))
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection '("marko-language-server" "--stdio"))
                      :activation-fn (lsp-activate-on "marko")
                      :server-id 'marko-language-server))))

(after! iedit
  (map! "C-:" 'iedit-mode))

(use-package! parrot
  :after rotate-text
  :config
  (defadvice! trigger-parrot (&rest _)
    :after-while #'rotate-text
    :after-while #'rotate-text-backward
    (parrot-start-animation))
  (parrot-mode))

(after! raku-mode
  (defun run-raku-return-buffer ()
    (interactive)
    (window-buffer (run-raku)))
  (set-repl-handler! 'raku-mode #'run-raku-return-buffer))

;; (use-package! symex
;;   :config
;;   (setq symex--user-evil-keyspec
;;         '(("j" . symex-go-up)
;;           ("k" . symex-go-down)
;;           ("C-j" . symex-climb-branch)
;;           ("C-k" . symex-descend-branch)
;;           ("M-j" . symex-goto-highest)
;;           ("M-k" . symex-goto-lowest)))
;;   (symex-initialize)
;;   ;; TODO: make use of doom macros maybe?
;;   ;; (evil-define-key 'normal symex-mode-map
;;   ;;   (kbd "<escape>") 'symex-mode-interface)
;;   ;; (evil-define-key 'insert symex-mode-map
;;   ;;   (kbd "<escape>") 'symex-mode-interface)
;;   ;; (map! :map symex-mode-map
;;   ;;       :ni "<escape>" #'symex-mode-interface)
;;   )

(setq-hook! 'typescript-mode-hook +format-with-lsp nil)
;; (add-hook 'vue-mode-hook #'lsp!)
;; TODO: lazy load this
(use-package! lsp-volar)


(set-docsets! 'c++-mode :add "Qt_5")
