;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


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
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 16 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

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
(map! :leader
      :desc "M-x"
      "SPC" 'counsel-M-x)
(map! :leader
      :desc "Search Dash"
      "ds" 'counsel-dash)
(map! :leader
      :desc "Lookup thing at point in Dash"
      "dd" 'counsel-dash-at-point)

(setq-default sly-lisp-implementations
              '((default ("ros" "run"))
                (cmu ("ros" "run" "--lisp" "cmu-bin"))
                (sbcl ("ros" "run" "--lisp" "sbcl-bin"))
                (abcl ("ros" "run" "--lisp" "abcl-bin"))
                (ccl ("ros" "run" "--lisp" "ccl-bin"))
                (ecl ("ros" "run" "--lisp" "ecl"))))

(setq-default sly-default-lisp 'default)

(add-to-list 'lispyville-key-theme 'text-objects)
(add-to-list 'lispyville-key-theme 'additional-motions)
(add-to-list 'lispyville-key-theme 'commentary)
(add-to-list 'lispyville-key-theme 'wrap)
(add-to-list 'lispyville-key-theme 'escape)

(put 'with 'lisp-indent-function 1)

;; (defun sly-qlot-exec (directory)
;;   (interactive (list (read-directory-name "Project directory: ")))
;;   (sly-start :program "qlot"
;;              :program-args '("exec" "ros" "-S" "." "run")
;;              :directory directory
;;              :name 'qlot
;;              :env (list (concat "PATH="
;;                                 (mapconcat 'identity exec-path ":"))
;;                         (concat "QUICKLISP_HOME="
;;                                 (file-name-as-directory directory) "quicklisp/"))))

(defun sly-qlot-exec (directory)
  (interactive (list (read-directory-name "Project directory: ")))
  (sly-start :program "qlot"
             :program-args '("exec" "ros" "-S" "." "run")
             :directory directory
             :name 'qlot
             :env (list (concat "PATH=" (mapconcat 'identity exec-path ":")))))

;; Spacemacs-like auto-paren skipping
(defun smart-closing-parenthesis ()
  (interactive)
  (let* ((sp-navigate-close-if-unbalanced t)
         (current-pos (point))
         (current-line (line-number-at-pos current-pos))
         (next-pos (save-excursion
                     (sp-up-sexp)
                     (point)))
         (next-line (line-number-at-pos next-pos)))
    (cond
     ((and (= current-line next-line)
           (not (= current-pos next-pos)))
      (sp-up-sexp))
     (t
      (insert-char ?\))))))

(map! :i
      :mode 'rustic-mode
      :after 'rustic-mode
      ")" 'smart-closing-parenthesis)
;; (define-key! evil-insert-state-map ")" 'smart-closing-parenthesis)

;; (add-to-list 'company-backends #'company-tabnine)
;; Default value: company-capf
;; (setq +lsp-company-backends '(company-tabnine company-capf))

(setq company-idle-delay 0)
(setq company-show-numbers t)

(setq lsp-csharp-server-path "/opt/omnisharp-roslyn-stdio/OmniSharp.exe")

;; (setq comp-deferred-compilation t)

;; We need this fix for some reason
(unless (fboundp 'cc-bytecomp-is-compiling)
  (defsubst cc-bytecomp-is-compiling ()
    "Return non-nil if eval'ed during compilation."
    (eq (cc-bytecomp-compiling-or-loading) 'compiling)))

(setq-default fill-column 120)

;; Counsel-dash
(setq-default dash-docs-docsets-path "~/.local/share/Zeal/Zeal/docsets/")
(setq-default dash-docs-browser-func 'browse-url)
(add-hook 'emacs-lisp-mode-hook (lambda () (setq-local dash-docs-docsets '("Emacs_Lisp"))))
(add-hook 'csharp-mode-hook (lambda () (setq-local dash-docs-docsets '("NET_Framework" "Unity_3D"))))
(add-hook 'rustic-mode-hook (lambda () (setq-local dash-docs-docsets '("Rust"))))

;; (require 'dap-gdb-lldb)
;; (require 'dap-lldb)
;; (setq dap-lldb-debug-program '("/usr/bin/lldb-vscode"))
;; (setq dap-lldb-debugged-program-function (lambda () (concatenate 'string (projectile-project-root) "target/debug/" (projectile-project-name))))

(setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

;; (lsp-register-client
;;  (make-lsp-client
;;   :new-connection (lsp-stdio-connection "/usr/bin/fsharp-language-server")
;;   :major-modes '(fsharp-mode)
;;   :server-id 'fsharp-lsp
;;   :notification-handlers (ht ("fsharp/startProgress" #'ignore)
;;                              ("fsharp/incrementProgress" #'ignore)
;;                              ("fsharp/endProgress" #'ignore))
;;   :priority 1))
