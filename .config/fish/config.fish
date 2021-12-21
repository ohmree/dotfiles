replay source /etc/profile

if status is-interactive
    keychain --agents ssh,gpg --eval --quiet --quick OhmRee | source
    # keychain --agents gpg --eval --quiet --quick OhmRee | source
    # gpg --quiet --decrypt ~/.secrets/twitch.sh.gpg | source

    if set -q INSIDE_EMACS && test $INSIDE_EMACS = 'vterm'
        # Disable vi when running inside emacs vterm-mode
        set -g fish_key_bindings fish_default_key_bindings
        if set -q EMACS_VTERM_PATH && test -f $EMACS_VTERM_PATH/etc/emacs-vterm.fish
            source $EMACS_VTERM_PATH/etc/emacs-vterm.fish
        end

        set_emacs_colors

        set -gx MCFLY_KEY_SCHEME emacs
    else
        set -gx MCFLY_KEY_SCHEME vim
    end
    set -gx MCFLY_FUZZY 2
    set -gx MCFLY_RESULTS 50
    mcfly init fish | source
end

if status is-login
    source ~/.profile
end

# tabtab source for packages
# uninstall by removing these lines
[ -f ~/.config/tabtab/fish/__tabtab.fish ]; and . ~/.config/tabtab/fish/__tabtab.fish; or true

if command -s opam > /dev/null
    eval (opam env)
end

function __direnv_export_eval --on-event fish_prompt;
    "/usr/bin/direnv" export fish | source;

    if test "$direnv_fish_mode" != "disable_arrow";
        function __direnv_cd_hook --on-variable PWD;
            if test "$direnv_fish_mode" = "eval_after_arrow";
                set -g __direnv_export_again 0;
            else;
                "/usr/bin/direnv" export fish | source;
            end;
        end;
    end;
end;

function __direnv_export_eval_2 --on-event fish_preexec;
    if set -q __direnv_export_again;
        set -e __direnv_export_again;
        "/usr/bin/direnv" export fish | source;
        echo;
    end;

    functions --erase __direnv_cd_hook;
end;
zoxide init fish | source
