#!/usr/bin/env bash
#
# Configures bash to interact with emacs
# supports:
# - vterm
# - M-x shell
# - term
# - shell from outside emacs
#
## functions
## Alias    Description         FROM TERM                   FROM EMACS GUI SHELL/TERM    FROM VTERM (remote is supported)
#  e        edit                Send file to GUI, retruns   Send to server, jumps to it  ffow on file
# se        sudo edit           Send file to GUI, retruns   Send to server, jumps to it  ffow on file
#  f        file                Open in terminal            Send to server, jumps to it  ffow on file
#  ff       find-file           Open in terminal            Send to server, jumps to it  ffow on file
# sf        sudo file           Open in terminal            Send to server, jumps to it  ffow on file
# sff       sudo find-file      Open in terminal            Send to server, jumps to it  ffow on file
# eshell    eshell              Open in terminal            Send to server, jumps to it  Impossible from remotes
### MISC:
# elisp             run lisp script in running emacs server
# to_buff           read pipeline and send it to a emacs buffer

# Emacs client
export ALTERNATE_EDITOR=""
export EMACS_SUDO_PREFIX="/sudo::"

_emacs_sudo () {
    echo "$@" | xargs -n1 realpath | sed -e "s;^;$EMACS_SUDO_PREFIX;" | xargs >&2
    echo "$@" | xargs -n1 realpath | sed -e "s;^;$EMACS_SUDO_PREFIX;" | xargs
}

_emacs_edit_from_inside () {
    for file in "$@"
    do
        # VTERM support
        if [[ -n ${EMACS_VTERM_PATH} ]]
        then
            vterm_cmd find-file-other-window "$file"
        else
            emacsclient -n -e "(find-file-other-window \"$file\")"
        fi
    done
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


# Pipe STDIN to an emacs buffer
to_buff () {
    # STDIN storage
    tobufftmp="$(mktemp)"
    trap 'rm -f -- "tobufftmp"' RETURN
    # Reading STDIN
    > "$tobufftmp" cat -
    if [ -n "$INSIDE_EMACS" ]; then
        # VTERM support
        if [[ -n ${EMACS_VTERM_PATH} ]]
        then
            vterm_cmd find-file-other-window "$tobufftmp"
        else
            # xargs is there to strip the "" from the beginning
            # and end of the output from Emacs.
            emacsclient -n "$tobufftmp" | xargs
        fi
    else
        emacsclient -t "$tobufftmp"
    fi

}

if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
    source "${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh"
fi
