;;; lang/forth/config.el -*- lexical-binding: t; -*-

;;
;; packages

(use-package! forth-mode
  :hook (lisp-mode-local-vars . sly-editing-mode)
  :config
  (set-repl-handler! 'forth-mode #'run-forth)
  (set-eval-handler! 'forth-mode #'forth-eval-region)
  (set-lookup-handlers! 'forth-mode
    :documentation #'forth-spec-lookup-2012)

  (map!
        ;; FIXME
        ;; (:map forth-mode-map
        ;;  :n "[d" #'forth-beginning-of-colon-definition
        ;;  :n "]d" #'forth-end-of-colon-definition)
        (:map forth-mode-map
         :n "[x" #'forth-beginning-of-colon-definition
         :n "]x" #'forth-end-of-colon-definition)

        (:localleader
         :map lisp-mode-map
         :desc "Sly"          "'" #'sly
         :desc "Sly (ask)"    ";" (cmd!! #'sly '-)
         :desc "Expand macro" "m" #'macrostep-expand
         (:prefix ("c" . "compile")
          :desc "Compile file"          "c" #'sly-compile-file
          :desc "Compile/load file"     "C" #'sly-compile-and-load-file
          :desc "Compile toplevel form" "f" #'sly-compile-defun
          :desc "Load file"             "l" #'sly-load-file
          :desc "Remove notes"          "n" #'sly-remove-notes
          :desc "Compile region"        "r" #'sly-compile-region)
         (:prefix ("e" . "evaluate")
          :desc "Evaluate buffer"     "b" #'sly-eval-buffer
          :desc "Evaluate last"       "e" #'sly-eval-last-expression
          :desc "Evaluate/print last" "E" #'sly-eval-print-last-expression
          :desc "Evaluate defun"      "f" #'sly-eval-defun
          :desc "Undefine function"   "F" #'sly-undefine-function
          :desc "Evaluate region"     "r" #'sly-eval-region)
         (:prefix ("g" . "goto")
          :desc "Go back"              "b" #'sly-pop-find-definition-stack
          :desc "Go to"                "d" #'sly-edit-definition
          :desc "Go to (other window)" "D" #'sly-edit-definition-other-window
          :desc "Next note"            "n" #'sly-next-note
          :desc "Previous note"        "N" #'sly-previous-note
          :desc "Next sticker"         "s" #'sly-stickers-next-sticker
          :desc "Previous sticker"     "S" #'sly-stickers-prev-sticker)
         (:prefix ("h" . "help")
          :desc "Who calls"               "<" #'sly-who-calls
          :desc "Calls who"               ">" #'sly-calls-who
          :desc "Lookup format directive" "~" #'hyperspec-lookup-format
          :desc "Lookup reader macro"     "#" #'hyperspec-lookup-reader-macro
          :desc "Apropos"                 "a" #'sly-apropos
          :desc "Who binds"               "b" #'sly-who-binds
          :desc "Disassemble symbol"      "d" #'sly-disassemble-symbol
          :desc "Describe symbol"         "h" #'sly-describe-symbol
          :desc "HyperSpec lookup"        "H" #'sly-hyperspec-lookup
          :desc "Who macro-expands"       "m" #'sly-who-macroexpands
          :desc "Apropos package"         "p" #'sly-apropos-package
          :desc "Who references"          "r" #'sly-who-references
          :desc "Who specializes"         "s" #'sly-who-specializes
          :desc "Who sets"                "S" #'sly-who-sets)
         (:prefix ("r" . "repl")
          :desc "Clear REPL"         "c" #'sly-mrepl-clear-repl
          :desc "Quit connection"    "q" #'sly-quit-lisp
          :desc "Restart connection" "r" #'sly-restart-inferior-lisp
          :desc "Sync REPL"          "s" #'sly-mrepl-sync)
         (:prefix ("s" . "stickers")
          :desc "Toggle breaking stickers" "b" #'sly-stickers-toggle-break-on-stickers
          :desc "Clear defun stickers"     "c" #'sly-stickers-clear-defun-stickers
          :desc "Clear buffer stickers"    "C" #'sly-stickers-clear-buffer-stickers
          :desc "Fetch stickers"           "f" #'sly-stickers-fetch
          :desc "Replay stickers"          "r" #'sly-stickers-replay
          :desc "Add/remove sticker"       "s" #'sly-stickers-dwim)
         (:prefix ("t" . "trace")
          :desc "Toggle"         "t" #'sly-toggle-trace-fdefinition
          :desc "Toggle (fancy)" "T" #'sly-toggle-fancy-trace
          :desc "Untrace all"    "u" #'sly-untrace-all)))

  (when (featurep! :editor evil +everywhere)
    (add-hook 'sly-mode-hook #'evil-normalize-keymaps)))


(use-package! sly-repl-ansi-color
  :defer t
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color))
