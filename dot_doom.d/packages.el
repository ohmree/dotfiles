;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

; (package! sly-named-readtables)
;; (package! sly-package-inferred :recipe (:host github :repo "40ants/sly-package-inferred"))
;; (package! sly-quicklisp)
;; (package! sly-asdf)
; (package! gnu-apl-mode)
(package! pkgbuild-mode)

;; (package! realgud-lldb)
; (package! meson-mode)

;; (package! flycheck-xo
;;  :recipe (:host github :repo "alexmurray/flycheck-xo"))

;; (package! magit-delta)
(package! keychain-environment)

;; (package! floobits
;;   :recipe (:host github :repo "ohmree/floobits-emacs"))

; (package! rust-playground)

; (package! keycast)
; (package! gif-screencast)

(package! elcord)

; (package! keypression)

; (package! ox-moderncv :recipe (:host gitlab :repo "Titan-C/org-cv" :branch "master"))
(package! parrot :recipe (:host github :repo "ohmree/parrot" :branch "master" :files (:defaults "img")))

;; (package! crdt)
;; (package! symex)
;; (package! lsp-volar :recipe (:host github :repo "jadestrong/lsp-volar"))
;; (package! vterm
;;   :pin "a940dd2ee8a82684860e320c0f6d5e15d31d916f")

(package! pacfiles-mode)
;; (package! vertico-posframe :recipe (:host github :repo "tumashu/vertico-posframe" :branch "main"))

(when (and (featurep! :editor evil)
           (featurep! :ui tree-sitter))
  (package! evil-textobj-tree-sitter))
  ;; (package! evil-textobj-tree-sitter
  ;;   :recipe (:host github
  ;;            :repo "meain/evil-textobj-tree-sitter"
  ;;            :files (:defaults "queries")))

;; (package! magit-gitflow :disable t)
(unpin! lsp-mode json-mode)

(package! org-modern)
;; (package! multi-vterm)

(package! polymode)
(package! poly-astro :recipe (:host github :repo "pablo-abc/poly-astro" :branch "main"))
