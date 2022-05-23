;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(use-package! auth-source)

(use-package! elcord
  :config
  (setq
   elcord-buffer-details-format-function
   (lambda ()
     "Return the buffer details string shown on discord, with a few tweaks."
     (pcase (buffer-name)
       ("*ielm*" "Inside IELM")
       ("*info*" "Reading info")
       ("*lsp-help*" "Reading help")
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
       (_ (elcord-buffer-details-format)))))
  (add-to-list 'elcord-boring-buffers-regexp-list "\\*doom\\*")
  (elcord-mode))

(use-package! pkgbuild-mode
  :mode "/PKGBUILD\\'")


(use-package! keychain-environment
  :config
  (keychain-refresh-environment))


;; TODO: fix this
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "me :)")

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
(setq doom-font (font-spec :family "JetBrains Mono" :size 16)
      doom-variable-pitch-font (font-spec :family "SF Pro Display")
      doom-unicode-font (font-spec :family "Noto Sans Hebrew"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


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

(put 'with 'lisp-indent-function 1)

(after! sly
  (map!
   :mode 'sly-mode
   (:prefix "]"
    :no "e" #'next-error)
   (:prefix "["
    :no "e" #'previous-error))
  (setq sly-lisp-implementations
        `((cmu ("clpm" "bundle" "exec" "--with-client" "cmucl"))
          (sbcl ("clpm" "bundle" "exec" "--with-client" "sbcl"))
          (abcl ("clpm" "bundle" "exec" "--with-client" "abcl"))
          (ccl ("clpm" "bundle" "exec" "--with-client" "ccl"))
          (ecl ("clpm" "bundle" "exec" "--with-client" "ecl"))))
  (setq sly-default-lisp 'sbcl))

(setq-default fill-column 120)

(after! dash-docs
  (setq dash-docs-docsets-path "~/.local/share/Zeal/Zeal/docsets/"
        dash-docs-browser-func #'eww))

(after! magit
  ;; (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  (setq magit-revision-show-gravatars t))

;; (use-package! magit-delta
;;   :after magit
;;   :hook (magit-mode . magit-delta-mode))



(after! lsp-mode
  (setq lsp-enable-folding t
        lsp-enable-text-document-color t
        lsp-headerline-breadcrumb-enable t
        ;; TODO: figure out if we need this with tree-sitter
        lsp-semantic-tokens-enable nil))
(after! lsp-headerline
  (setq lsp-headerline-breadcrumb-segments '(symbols)))
(after! lsp-elixir
  (setq lsp-elixir-server-command '("/usr/bin/elixir-ls")))
(after! lsp-treemacs
  (add-hook 'lsp-treemacs-generic-mode-hook #'lsp-treemacs-sync-mode))

(after! lsp-toml
  (setq lsp-toml-schema-links t))

(after! lsp-css
  (add-to-list 'lsp-css-experimental-custom-data (concat doom-private-dir "postcss.css-data.json")))

(after! lsp-rust
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-experimental-proc-attr-macros t
        lsp-rust-analyzer-proc-macro-enable t
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-target-dir "/tmp/lsp-rust-target"))

(after! rustic
  (add-hook 'rustic-mode-hook #'lsp-rust-analyzer-inlay-hints-mode))

;; (after! lsp-clangd
;;   (setq lsp-clients-clangd-args '("-j=3"
;;                                   "--background-index"
;;                                   "--clang-tidy"
;;                                   "--completion-style=detailed"
;;                                   "--header-insertion=never"
;;                                   "--header-insertion-decorators=0"))
;;   (set-lsp-priority! 'clangd 2))

(after! lsp-lua
  (setq lsp-clients-lua-language-server-install-dir "/usr/lib/lua-language-server"
        lsp-clients-lua-language-server-bin "/usr/bin/lua-language-server"
        lsp-clients-lua-language-server-main-location "/usr/lib/lua-language-server/main.lua"))

(custom-set-faces!
  '(aw-leading-char-face
    :foreground "white" :background "red"
    :weight bold :height 2.5 :box (:line-width 10 :color "red")))

(after! fuel-mode
  (setq fuel-factor-root-dir "/usr/lib/factor"))

(setq enable-local-variables t)

(after! org
  (setq org-use-property-inheritance t))

(after! ox-latex
  (setq org-latex-compiler "lualatex"))

;; (use-package! ox-moderncv)

(after! iedit
  (map! "C-:" 'iedit-mode))

(quiet!
 (use-package! parrot
   :config
   (defadvice! trigger-parrot (&rest _)
     :after-while #'rotate-text
     :after-while #'rotate-text-backward
     (parrot-start-animation))
   (parrot-set-parrot-type 'bat)
   (parrot-mode)))

(after! raku-mode
  (set-repl-handler!
    'raku-mode
    (lambda ()
      (interactive)
      (window-buffer (run-raku)))))

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

;; (after! lsp-volar
;;   (setq lsp-volar-take-over-mode nil))

(after! lsp-eslint
  (setq lsp-eslint-package-manager "pnpm"))

(after! cc-mode
  (set-docsets! 'c++-mode :add "Qt_5"))


(use-package! pacfiles-mode)

;; (use-package! vertico-posframe
;;   :after vertico
;;   :config
;;   (setq vertico-posframe-width 120
;;         ;; vertico-posframe-min-width 90
;;         vertico-posframe-border-width 0
;;         vertico-posframe-min-height vertico-count
;;         ;; vertico-posframe-height vertico-count
;;         vertico-posframe-poshandler #'posframe-poshandler-frame-top-center)
;;   (vertico-posframe-mode))
;; (add-hook 'vertico-posframe-mode-hook #'solaire-mode-fix-minibuffer)

(after! fish-mode
  (set-company-backend! 'fish-mode 'company-fish-shell 'company-yasnippet))

(after! emojify
  (setq emojify-display-style 'unicode))

(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'stack))

(setq +treemacs-git-mode 'deferred)

(after! doom-themes-ext-treemacs
  (setq doom-themes-treemacs-theme "doom-colors"))

(after! web-mode
  (setq web-mode-enable-comment-annotation t))

(setq-hook! '(typescript-mode-hook rjsx-mode-hook)
  +format-with-lsp nil)

(use-package! evil-textobj-tree-sitter
  :when (featurep! :editor evil)
  :after tree-sitter
  :config
  (map! (:map evil-inner-text-objects-map
         "f" (evil-textobj-tree-sitter-get-textobj "function.inner")
         "F" (evil-textobj-tree-sitter-get-textobj "call.inner")
         "C" (evil-textobj-tree-sitter-get-textobj "class.inner")
         "i" (evil-textobj-tree-sitter-get-textobj "conditional.inner")
         "l" (evil-textobj-tree-sitter-get-textobj "loop.inner"))
        (:map evil-outer-text-objects-map
         "f" (evil-textobj-tree-sitter-get-textobj "function.outer")
         "F" (evil-textobj-tree-sitter-get-textobj "call.outer")
         "C" (evil-textobj-tree-sitter-get-textobj "class.outer")
         "c" (evil-textobj-tree-sitter-get-textobj "comment.outer")
         "i" (evil-textobj-tree-sitter-get-textobj "conditional.outer")
         "l" (evil-textobj-tree-sitter-get-textobj "loop.outer"))))

;;; :app everywhere
(after! emacs-everywhere
  (remove-hook 'emacs-everywhere-init-hooks #'hide-mode-line-mode))

;; Semi-center it over the target window, rather than at the cursor position
;; (which could be anywhere).
;; (defadvice! center-emacs-everywhere-in-origin-window (frame window-info)
;;   :override #'emacs-everywhere-set-frame-position
;;   (cl-destructuring-bind (x y width height)
;;       (emacs-everywhere-window-geometry window-info)
;;     (set-frame-position frame
;;                         (+ x (/ width 2) (- (/ width 2)))
;;                         (+ y (/ height 2))))))

(after! json-mode
  (add-to-list 'auto-mode-alist '("\\(?:\\(?:tsconfig\\(?:\\.[^.]+\\)?\\.json\\)\\|\\(?:\\.vscode/.+\\.json\\)\\)\\'" . jsonc-mode)))

(after! grip-mode
  (setq grip-preview-use-webkit nil)
  (let* ((user "ohmree")
         (credential
          (car
           (auth-source-search
            :host "api.github.com"
            :user (format "%s^grip" user))))
         (token (plist-get credential :secret)))
    (setq grip-github-user user
          grip-github-password (funcall token))))

(after! doom-modeline
  (setq doom-modeline-github t
        doom-modeline-enable-word-count t
        doom-modeline-hud t
        doom-modeline-major-mode-icon t))

;; Fix `lsp-volar' warning.
(defadvice! +lsp--create-filter-function (workspace)
  :override #'lsp--create-filter-function
  (let ((body-received 0)
        leftovers body-length body chunk)
    (lambda (_proc input)
      (setf chunk (if (s-blank? leftovers)
                      input
                    (concat leftovers input)))

      (let (messages)
        (while (not (s-blank? chunk))
          (if (not body-length)
              ;; Read headers
              (if-let ((body-sep-pos (string-match-p "\r\n\r\n" chunk)))
                  ;; We've got all the headers, handle them all at once:
                  (setf body-length (lsp--get-body-length
                                     (mapcar #'lsp--parse-header
                                             (split-string
                                              (substring-no-properties chunk
                                                                       (or (string-match-p "Content-Length" chunk)
                                                                           (error "Unable to find Content-Length header."))
                                                                       body-sep-pos)
                                              "\r\n")))
                        body-received 0
                        leftovers nil
                        chunk (substring-no-properties chunk (+ body-sep-pos 4)))

                ;; Haven't found the end of the headers yet. Save everything
                ;; for when the next chunk arrives and await further input.
                (setf leftovers chunk
                      chunk nil))
            (let* ((chunk-length (string-bytes chunk))
                   (left-to-receive (- body-length body-received))
                   (this-body (if (< left-to-receive chunk-length)
                                  (prog1 (substring-no-properties chunk 0 left-to-receive)
                                    (setf chunk (substring-no-properties chunk left-to-receive)))
                                (prog1 chunk
                                  (setf chunk nil))))
                   (body-bytes (string-bytes this-body)))
              (push this-body body)
              (setf body-received (+ body-received body-bytes))
              (when (>= chunk-length left-to-receive)
                (condition-case err
                    (with-temp-buffer
                      (apply #'insert
                             (nreverse
                              (prog1 body
                                (setf leftovers nil
                                      body-length nil
                                      body-received nil
                                      body nil))))
                      (decode-coding-region (point-min)
                                            (point-max)
                                            'utf-8)
                      (goto-char (point-min))
                      (while (search-forward "\\u0000" nil t)
                        (replace-match "" nil t))
                      (goto-char (point-min))
                      (push (lsp-json-read-buffer) messages))

                  (error
                   (lsp-warn "Failed to parse the following chunk:\n'''\n%s\n'''\nwith message %s"
                             (concat leftovers input)
                             err)))))))
        (mapc (lambda (msg)
                (lsp--parser-on-message msg workspace))
              (nreverse messages))))))

(use-package! org-modern
  :after org
  :config
  (add-hook 'org-mode-hook #'org-modern-mode))

;; (use-package! multi-vterm
;;   :after vterm
;;   :config
;;   (map! :map vterm-mode-map
;;         :n ", c" 'multi-vterm
;;         :n ", n" 'multi-vterm-next
;;         :n ", p" 'multi-vterm-prev))

(after! lsp-javascript
  (setq lsp-clients-typescript-log-verbosity "off"))

;; (use-package! polymode)
(use-package! poly-astro
  :mode ("\\.astro\\'" . poly-astro)
  :defer-incrementally polymode
  :after polymode)
