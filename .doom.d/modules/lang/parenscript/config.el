;;; lang/parenscript/config.el -*- lexical-binding: t; -*-

(defer-feature! lisp-mode)

(after! trident-mode
  (set-docsets! 'trident-mode "Parenscript")

  (set-ligatures! 'trident-mode
    :def "defun"
    :lambda "lambda"
    :null "nil"
    :true "true" :false "false"
    :not "not"
    :and "and"
    :or "or"
    :for "loop"
    :return "return"
    :yield "use-package"))

(use-package! trident-mode
  :mode "\\.paren")
