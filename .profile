export PATH="$HOME/.emacs.d/bin:$HOME/.cargo/bin:$HOME/.yarn/bin:$PATH"
export EDITOR="emacsclient -qc -nw -a=''"
export GUI_EDITOR="emacsclient -nqc -a=''"
export VISUAL="${EDITOR}"
export MANPAGER=dash -c 'col -bx | bat -l man -p'
