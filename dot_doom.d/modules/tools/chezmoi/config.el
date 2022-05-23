;;; tools/chezmoi/config.el -*- lexical-binding: t; -*-

(use-package! chezmoi
  :commands (chezmoi-find chezmoi-write chezmoi-diff chezmoi-magit-status)
  :config
  (require 'cl)
  ;; Automatically update target files.
  (add-hook! after-save-hook :append
    (when chezmoi-buffer-target-file
      (chezmoi-write buffer-target-file))))

(defun +chezmoi/apply-all ()
  "Run =chezmoi apply= in a *compilation* buffer."
  (interactive)
  (autoload 'doom--if-compile
    (expand-file-name "core/autoload/config.el" user-emacs-directory)
    nil nil 'macro)
  (doom--if-compile "chezmoi apply"
      (message "Dotfiles applied successfully!")
    (message "Failed to apply dotfiles")))

(defun +chezmoi/update ()
  "Run =chezmoi update= in a *compilation* buffer."
  (interactive)
  (autoload 'doom--if-compile
    (expand-file-name "core/autoload/config.el" user-emacs-directory)
    nil nil 'macro)
  (doom--if-compile "chezmoi update"
      (message "Dotfiles updated successfully!")
    (message "Failed to update dotfiles")))

(map! :leader
      (:prefix ("d" . "dotfile")
       :desc "Write all dotfiles"  "a" #'+chezmoi/apply-all
       :desc "Dotfile diff"        "d" #'chezmoi-diff
       :desc "Find dotfile"        "f" #'chezmoi-find
       :desc "Dotfile repo status" "s" #'chezmoi-magit-status
       :desc "Update dotfiles"     "u" #'+chezmoi/update))
