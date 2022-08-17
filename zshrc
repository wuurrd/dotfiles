# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

########################
# Antigen
########################
source /usr/share/zsh/share/antigen.zsh

antigen use oh-my-zsh
antigen bundle StackExchange/blackbox
antigen bundle brew
antigen bundle command-not-found
antigen bundle common-aliases
antigen bundle direnv
antigen bundle docker
antigen bundle docker-compose
antigen bundle git
antigen bundle golang
antigen bundle npm
antigen bundle nvm
antigen bundle python
antigen bundle tmux
antigen theme romkatv/powerlevel10k
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zaw
antigen bundle history-substring-search
antigen apply

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

source ~/.aliases

bindkey '^R' zaw-history
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

export DOCKER_BUILDKIT=1
export COMPOSE_DOCKER_CLI_BUILD=1
export XDG_RUNTIME_DIR='/run/user/1000'

eval "$(pyenv init -)"
export EDITOR=vim
export PATH="$PATH:$HOME/bin:$HOME/.krew/bin"
eval $(ssh-agent -s) > /dev/null
ssh-add -k ~/.ssh/id_ed25519 2>/dev/null >/dev/null
export VAULT_PASSWORD="$(cat ~/.vault_password)"
