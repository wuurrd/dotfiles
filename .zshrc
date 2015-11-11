# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="xiong-chiamiov"
ZSH_THEME="ys"
#ZSH_THEME=candy

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle -e ':completion:*:approximate:*' \
        max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always
zstyle ':completion:*' special-dirs true
zstyle ':filter-select:highlight' matched fg=yellow,standout
zstyle ':filter-select' max-lines $(($LINES / 2))
zstyle ':filter-select' rotate-list yes # enable rotation for filter-select
zstyle ':filter-select' case-insensitive yes # enable case-insensitive search
zstyle ':filter-select' extended-search yes # see below

# Uncomment following line if you want red dots to be displayed while waiting for completion
#COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(gitfast pip fabric battery debian)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export LSCOLORS=Exfxcxdxbxegedabagacad
source ~/.aliases

export PATH="/usr/local/bin:/usr/local/sbin:/usr/local/share/npm/bin:/usr/local/texlive/2012/bin/universal-darwin:/sbin/:$HOME/bin:$PATH"
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export PATH="$HOME/dotfiles/bin:$PATH";

source ~/src/zaw/zaw.zsh
source ~/src/zsh-history-substring-search/zsh-history-substring-search.zsh


insert-root-prefix() {
  BUFFER="sudo $BUFFER"
  CURSOR=$(($CURSOR + 5))
}
zle -N insert-root-prefix

bindkey '^X^P' insert-root-prefix
bindkey '^R' zaw-history
bindkey '^X^F' zaw-git-branches
bindkey '^X^S' zaw-git-status
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

fpath=(/usr/local/share/zsh-completions $fpath)



function b (){
  local arg=${1:-1};
  local dir=""
  while [ $arg -gt 0 ]; do
    dir="../$dir"
    arg=$(($arg - 1));
  done
  cd $dir >&/dev/null
}

fancy-ctrl-z () {
  if [[ $#BUFFER -eq 0 ]]; then
    fg
    zle redisplay
  else
    zle push-input
    zle clear-screen
  fi
}
zle -N fancy-ctrl-z
bindkey '^Z' fancy-ctrl-z

if [[ -s '/etc/zsh_command_not_found' ]]; then
  source '/etc/zsh_command_not_found'
fi
export TERM="xterm-256color"
export EDITOR="vim"
export PEXDEV_FILESERVER=nofs1.rd.pexip.com
export PEX_CONFERENCING_CONFIG_DB="$HOME/src/mcu/resources/cfg/mcu/pexcfg.db"

check_stream () {
  ./gst-launch-0.10 rtmpsrc location="$1 live=1" ! flvdemux name=demux demux.video ! h264parse ! pexh264dec ! ffmpegcolorspace ! fakesink async=0 demux.audio ! fakesink async=0 -v
}

export MCUDIR=/home/dbu/src/mcu

#export PYTHONPATH=$MCUDIR:$MCUDIR/.build/__autotools__/linux-x86_64/lib/python2.7/site-packages/:$MCUDIR/.build/__autotools__/linux-x86_64/lib/python2.7/site-packages/gst-0.10/:$PYTHONPATH
#export LD_LIBRARY_PATH=$MCUDIR/.build/__autotools__/linux-x86_64/lib:$MCUDIR/.build/__autotools__/linux-x86_64/lib/gstreamer-0.10/
#export GST_PLUGIN_PATH=$MCUDIR/.build/__autotools__/linux-x86_64/lib/gstreamer-0.10/
#export GST_PLUGIN_SCANNER=$MCUDIR/.build/__autotools__/linux-x86_64/libexec/gstreamer-0.10/gst-plugin-scanner
#export FS_PLUGIN_PATH=$MCUDIR/.build/__autotools__/linux-x86_64/lib/farstream-0.1/
[ -f ~/dotfiles/zsh/`hostname`.sh ] && source ~/dotfiles/zsh/`hostname`.sh
export DONT_RESTART_NM=1
export PATH="/home/dbu/Android/Sdk/build-tools/22.0.1/:$PATH"
