#!/usr/bin/env bash
#
# Configures bash to interact with emacs
# supports:
# - vterm
# - M-x shell
# - term
# - shell from outside emacs
#
## Availables functions
#  ALIAS    DESCRIPTION         FROM EXTERNAL TERMINAL      FROM INSIDE EMACS
#  e        edit                Send file to GUI,           Send to server,
#                               returns                     jumps to it
# se        sudo edit           Send file to GUI,           Send to server,
#                               returns                     jumps to it
#  f        file                Open in terminal            Send to server,
#                                                           jumps to it
#  ff       find-file           Open in terminal            Send to server,
#                                                           jumps to it
# sf        sudo file           Open in terminal            Send to server,
#                                                           jumps to it
# sff       sudo find-file      Open in terminal            Send to server,
#                                                           jumps to it
# to_buff   send stdin to       Open Buffer                 Send to server,
#           temporary buffer    in terminal                 jumps to it
#
### MISC:
# elisp             run lisp script in running emacs server
# eshell            Start Eshell in your terminal
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
        if echo "$INSIDE_EMACS" | grep "vterm" &> /dev/null
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
    # export VISUAL="emacsclient -c -a emacs"    # $VISUAL opens in GUI mode
    alias  e=_emacs_edit_from_inside
    alias se=_emacs_sudo_edit_from_inside
    alias  f=_emacs_edit_from_inside
    alias sf=_emacs_sudo_edit_from_inside
    alias  ff=_emacs_edit_from_inside
    alias sff=_emacs_sudo_edit_from_inside

else
    # we don't have to worry about nested frames
    export EDITOR="emacsclient -t"              # $EDITOR opens in terminal
    # export VISUAL="emacsclient -c -a emacs"   # $VISUAL opens in GUI mode
    alias  e=_emacs_edit_from_inside            # open in emacs sever
    alias se=_emacs_sudo_edit_from_inside
    alias  f=_emacs_edit_from_outside           # open in terminal
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

# Emacs shell
shell () {
    $EDITOR -e "(+shell/here)"
}

# Emacs shell
vterm () {
    $EDITOR -e "(+vterm/here t)"
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
            return 0
        else
            # xargs is there to strip the "" from the beginning
            # and end of the output from Emacs.
            emacsclient -n "$tobufftmp" | xargs
            elisp '(persp-add-buffer (current-buffer))'
            return 0
        fi
    else
        emacsclient -t "$tobufftmp"
            return 0
    fi

}

# Pipe STDIN to an Clipboard
to_clip () {

    if ! which xclip > /dev/null
    then
        echo "xclip not found"
        return 1
    fi
    cat - | xclip -in -selection clipboard
    return 0

}

# START:

alias k=kubectl
alias etcdctl="kubectl exec etcd-$HOSTNAME -- etcdctl --cacert /etc/kubernetes/pki/etcd/ca.crt --cert /etc/kubernetes/pki/etcd/server.crt     --key /etc/kubernetes/pki/etcd/server.key"
#
if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
    source "${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh"
fi
