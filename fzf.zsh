_fzf_alist() {
    fc -l 1| sed 's/ *[0-9]* *//' | while read line; do
        for v in "${(z)line}"; do
            if [[ ${#v} -gt 6 ]]; then
                echo "$v"
            fi
        done
    done
}

_fzf_asel() {
    CURWORD="${${(z)1}[-1]}"
    PREFIX_END="$((-${#CURWORD} - 1))"
    PREFIX="${1[0,$PREFIX_END]}"
    setopt localoptions pipefail 2> /dev/null
    NEWWORD=$(_fzf_alist | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse $FZF_DEFAULT_OPTS" $(_fzfcmd) -q "$CURWORD" --tiebreak index --tac)
    if [ "$?" -eq 0 ]; then
        echo "$PREFIX$NEWWORD"
        return 0
    else
        echo "$1"
        return 1
    fi
    return $ret
}

_fzf_use_tmux() {
    [ -n "$TMUX_PANE" ] && [ "${FZF_TMUX:-0}" != 0 ] && [ ${LINES:-40} -gt 15 ]
}

_fzfcmd() {
    _fzf_use_tmux &&
        echo "fzf-tmux -d${FZF_TMUX_HEIGHT:-40%}" || echo "fzf"
}

fzf-argument-widget() {
    LBUFFER="$(_fzf_asel "$LBUFFER")"
    local ret=$?
    zle redisplay
    typeset -f zle-line-init >/dev/null && zle zle-line-init
    return $ret
}
zle     -N   fzf-argument-widget
bindkey '^[,' fzf-argument-widget
