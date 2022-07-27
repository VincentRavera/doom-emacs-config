#!/usr/bin/env bash
#
# Configures terminal to interact with emacs
#
## functions
## Alias    Description         FROM TERM                   FROM GUI
#  e        edit                Send file to GUI, retruns   Send to server, jumps to it
# se        sudo edit           Send file to GUI, retruns   Send to server, jumps to it
#  f        file                Open in terminal            Send to server, jumps to it
#  ff       find-file           Open in terminal            Send to server, jumps to it
# sf        sudo file           Open in terminal            Send to server, jumps to it
# sff       sudo find-file      Open in terminal            Send to server, jumps to it
# eshell    eshell              Open in terminal            Send to server, jumps to it
### MISC:
# elisp             run lisp script in running emacs server

# Emacs client
export ALTERNATE_EDITOR=""
export EMACS_SUDO_PREFIX="/sudo::"

_emacs_sudo () {
    echo "$@" | xargs -n1 realpath | sed -e "s;^;$EMACS_SUDO_PREFIX;" | xargs >&2
    echo "$@" | xargs -n1 realpath | sed -e "s;^;$EMACS_SUDO_PREFIX;" | xargs
}

_emacs_edit_from_inside () {
    emacsclient -n "$@"
}

_emacs_edit_from_outside () {
    emacsclient -t "$@"
}

_emacs_sudo_edit_from_inside () {
    _emacs_edit_from_inside "$(_emacs_sudo "$@")"
}

_emacs_sudo_edit_from_outside () {
    _emacs_edit_from_outside "$(_emacs_sudo "$@")"
}


if [ -n "$INSIDE_EMACS" ]; then
    # we are interacting from a shell/term inside of Emacs
    # we do not want to use --tty since we will have nested emacs frames
    export EDITOR="emacsclient"                  # $EDITOR opens in terminal
    # export VISUAL="emacsclient -c -a emacs"         # $VISUAL opens in GUI mode
    alias  e=_emacs_edit_from_inside
    alias se=_emacs_sudo_edit_from_inside
    alias  f=_emacs_edit_from_inside
    alias sf=_emacs_sudo_edit_from_inside
    alias  ff=_emacs_edit_from_inside
    alias sff=_emacs_sudo_edit_from_inside

else
    # we don't have to worry about nested frames
    export EDITOR="emacsclient -t"                  # $EDITOR opens in terminal
    # export VISUAL="emacsclient -c -a emacs"         # $VISUAL opens in GUI mode
    alias  e=_emacs_edit_from_inside            # will send the file to server and return
    alias se=_emacs_sudo_edit_from_inside
    alias  f=_emacs_edit_from_outside            # will open the file in the current terminal
    alias sf=_emacs_sudo_edit_from_outside
    alias  ff=_emacs_edit_from_outside
    alias sff=_emacs_sudo_edit_from_outside
fi


# Emacs lisp execute
elisp () {
    emacsclient -n -t -e "$@"
}

# Emacs eshell
eshell () {
    $EDITOR -e "(+eshell/here)"
}

vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

vterm_prompt_end(){
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}

# black="\[\e[0;30m\]"
# red="\[\e[0;31m\]"
green="\[\e[0;32m\]"
yellow="\[\e[0;33m\]"
blue="\[\e[0;34m\]"
# purple="\[\e[0;35m\]"
# cyan="\[\e[0;36m\]"
# white="\[\e[0;37m\]"
# orange="\[\e[0;91m\]"
normal="\[\e[0m\]"
# reset_color="\[\e[39m\]"
BASIC_PS1="${green}\u${normal}@${blue}\H${normal}:${yellow}\w${normal} ${green}\$${normal} "
# echo "----$PS1----"
PS1="$BASIC_PS1"
PS1=$PS1'\[$(vterm_prompt_end)\]'
