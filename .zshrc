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
plugins=(gitfast osx brew pip fabric battery sublime)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export LSCOLORS=Exfxcxdxbxegedabagacad
source ~/.aliases

export PATH="/usr/local/bin:/usr/local/sbin:/usr/local/share/npm/bin:/usr/local/texlive/2012/bin/universal-darwin:$PATH"
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export HOMEBREW_GITHUB_API_TOKEN="cae72932a8b1d5f855a5e36a0559a6d412b776a3"

export PERL_LOCAL_LIB_ROOT="$PERL_LOCAL_LIB_ROOT:/Users/davbuchm/perl5";
export PERL_MB_OPT="--install_base /Users/davbuchm/perl5";
export PERL_MM_OPT="INSTALL_BASE=/Users/davbuchm/perl5";
export PERL5LIB="/Users/davbuchm/perl5/lib/perl5:$PERL5LIB";
export PATH="/Users/davbuchm/perl5/bin:$PATH";

export NPM_CONFIG_REGISTRY="http://npm-mirror.rusclabs.cisco.com/"
export NODE_NO_READLINE=1
export PHANTOMJS_BIN="/usr/local/bin/phantomjs"

[[ -s `brew --prefix`/etc/autojump.sh ]] && . `brew --prefix`/etc/autojump.sh


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
