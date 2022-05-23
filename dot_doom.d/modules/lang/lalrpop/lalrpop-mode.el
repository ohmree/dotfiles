;;; lang/lalrpop/lalrpop-mode.el -*- lexical-binding: t; -*-

(defgroup lalrpop-mode nil
  "Support for lalrpop grammar files"
  :group 'languages)

(defcustom lalrpop-indent-offset 4
  "Indent blocks in lalrpop mode by this number of spaces"
  :type 'integer
  :group 'rustic-mode
  :safe #'integerp)

(defvar lalrpop-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\/ ". 12b" table)
    (modify-syntax-entry ?\n "> b" table)

    table))

(defconst lalrpop-mode-keywords
  '(
    "enum"
    "extern"
    "grammar"
    "pub"
    "type"
    "use"
    ))

(defconst lalrpop-rule-name-regexp
  (rx symbol-start
      (group upper (0+ (any word nonascii digit "_")))
      symbol-end))

(defconst lalrpop-rule-name-captured-regexp
  (rx (any ":" "<")
      (zero-or-more (any whitespace))
      (group upper (0+ (any word nonascii digit "_")))
      (zero-or-more (any whitespace))
      ">"))

(defconst lalrpop-capture-ident-regexp
  (rx "<"
      (zero-or-more (any whitespace))
      (group (0+ (any lower digit "_")))
      (zero-or-more (any whitespace))
      ":"))

(defun lalrpop-mode-syntactic-face-function (state)
 "Syntactic face function to distinguish doc comments from other comments."
 (if (nth 3 state) 'font-lock-string-face
     (save-excursion
       (goto-char (nth 8 state))
       (if (looking-at "/\\([*][*!][^*!]\\|/[/!][^/!]\\)")
           'font-lock-doc-face
         'font-lock-comment-face
     ))))


(setq lalrpop-font-lock-keywords
      (let* (
             (x-keywords-regexp (regexp-opt lalrpop-mode-keywords 'symbols)))
        `(
          (,x-keywords-regexp . font-lock-keyword-face)
          (,lalrpop-rule-name-regexp 1 font-lock-function-name-face)
          (,lalrpop-rule-name-captured-regexp 1 font-lock-function-name-face)
          (,lalrpop-capture-ident-regexp 1 font-lock-variable-name-face)
          (,(regexp-opt '("=>") 'symbols) . font-lock-negation-char-face)
              )))

;;;###autoload
(define-derived-mode lalrpop-mode prog-mode "LALRPOP mode"
  "Major mode for editing lalrpop grammar definitions"
  :group 'lalrpop-mode
  :syntax-table lalrpop-mode-syntax-table

  (setq-local font-lock-defaults '(lalrpop-font-lock-keywords
                                   nil nil nil nil
                                   (font-lock-syntactic-face-function . lalrpop-mode-syntactic-face-function)))
  (setq-local comment-start "//")
  (setq-local comment-end "")

  (setq-local comment-multi-line t)
  (setq-local comment-line-break-function 'lalrpop-comment-indent-new-line)

  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lalrpop\\'" . lalrpop-mode))

(provide 'lalrpop-mode)
