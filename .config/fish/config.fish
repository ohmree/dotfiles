if status --is-login
    bax source /etc/profile
    bax source ~/.profile
end

if status --is-interactive
    eval (keychain --eval --quiet --quick ~/.ssh/id_rsa)
end

