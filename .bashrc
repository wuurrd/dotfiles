#!/bin/bash
# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
export HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
if [ "$TERM" = "xterm" ]; then
    export TERM="xterm-256color"
fi

case "$TERM" in
    xterm* | screen) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

#if [ "$color_prompt" = yes ]; then
#    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
#else
#    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
#fi
#unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*|screen*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

source ~/dotfiles/.bash_alias

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto --exclude-dir .svn --exclude-dir .git'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'
alias sl='ls -CF'
alias lsd='ls -CF'

# enable programmable completion features (you don't need to enable<
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

function .. (){
  local arg=${1:-1};
  local dir=""
  while [ $arg -gt 0 ]; do
    dir="../$dir"
    arg=$(($arg - 1));
  done
  cd $dir >&/dev/null
}


if [ "$color_prompt" = yes ]; then
t=
[ -z "$TERM" ] && t="-T xterm"

EN=$(tput $t enacs)
SM=$(tput $t smacs)
RM=$(tput $t rmacs)

__svn_rev ()
{
	LANG='C' svn info 2>/dev/null | awk '/Revision:/ {print $2; }'
}

__svn_last_changed ()
{
	LANG='C' svn info 2>/dev/null | awk '/Last Changed Rev:/ { print $4;}'
}


__svn_clean ()
{
	STATE=$(LANG='C' svn -N status -q 2>/dev/null | grep "^[MA]" | wc -l)
	if [ $STATE != 0 ]; then
		echo "$2"
	else
		echo "$1"
	fi
}
__svn_remote_clean ()
{
	STATE=$(LANG='C' svn -N status -u -q 2>/dev/null | egrep  " *\*" | wc -l)
	if [ $STATE != 0 ]; then
		echo "$2"
	else
		echo "$1"
	fi
}

__svn_stat ()
{
	[ -d .svn ] || return
	REV=$(__svn_rev)
	if [ $REV ]; then
		if [ $SVNP_CHECK_DISTANT_REPO ]; then
			REMOTE_STATUS=$(__svn_remote_clean "" "*")
		fi
		LOCAL_STATUS=$(__svn_clean "" "*")
		echo $REMOTE_STATUS$REV$LOCAL_STATUS
	fi
}

git_branch () {
    br=$(__git_ps1)
    svnbr=$(__svn_stat)
    if [ -n "$br" ]; then
        echo "${SM}qqq["$'\e[1;36m'"${RM}${br:2:$((${#br}-3))}${SM}"$'\e[1;37m'"]"
    elif [ "$svnbr" != "" ]; then
        echo "${SM}qqq["$'\e[1;36m'"${RM}$svnbr${SM}"$'\e[1;37m'"]"
    else
        echo -n ""
    fi
}
COUNTER=0
LIMIT=10
get_header() {
    echo -e "\e[1;37m${EN}${SM}lqqqq[${RM}\e[1;36m$HOSTNAME\e[1;32m:$PWD\e[1;37m${SM}]$(git_branch)qqqq~${RM}"
    return
}

PROMPT_COMMAND="get_header;$PROMPT_COMMAND"

PS1="\[\e[1;37m\]\[$SM\]mq\[$RM\][\[\e[0m\]\[\e[1;35m\]\\t\[\e[1;37m\]]\[$SM\]q~~ ~\[$RM\] \$\[\e[0m\] "
elif [ "$TERM" = "eterm-color" ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
elif [ "$TERM" = "screen-256color" ]; then

    function _update_ps1()
    {
       export PS1="$(~/dotfiles/powerline-bash/powerline-bash.py $?)"
    }
    export PROMPT_COMMAND="_update_ps1"
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt
complete -W "$(echo $(grep '^ssh ' ~/.bash_history | sort -u | sed 's/^ssh //'))" ssh
complete -W "$(echo $(grep '^telnet ' ~/.bash_history | sort -u | sed 's/^telnet //'))" telnet
_fab_completion()
{
    local cur prev opts

        COMPREPLY=()
        cur="${COMP_WORDS[COMP_CWORD]}"
        prev="${COMP_WORDS[COMP_CWORD-1]}"

# Get the list of available tasks, ignoring errors (missing or invalid fabfile)
# Removing the "Available commands" header and any empty line or spaces
        opts="`fab --list 2> /dev/null | grep -v -e 'Available commands' -e "^$" | sed -e 's/ \+\(.\+\)/\1/g'`"

        COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
        return 0
}
complete -F _fab_completion -o default fab
export NOSE_REDNOSE=1

export LESSOPEN="| /usr/share/source-highlight/src-hilite-lesspipe.sh %s"
export LESS=' -R '

#[[ $- != *i* ]] && return
#[[ $TERM != "screen" ]] && (tmux -q has-session && tmux attach-session) || tmux
