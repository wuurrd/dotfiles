#! /bin/sh
pushd .
mkdir -p ~/src
cd ~/src
git clone git@github.com:zsh-users/zaw.git
git clone git@github.com:zsh-users/zsh-history-substring-search.git

cd ~
git clone git://github.com/robbyrussell/oh-my-zsh.git

popd # back to ~ (in zsh cd'ing popd / pushd's)
popd # back to dotfiles
#git clone https://github.com/pdf/ubuntu-mono-powerline-ttf.git ~/.fonts/ubuntu-mono-powerline-ttf
#fc-cache -vf

#ln -s `pwd`/irssi ~/.irssi

###### LINUX ######
#ln -s `pwd`/fluxbox ~/.fluxbox
#ln -s `pwd`/awesome ~/.config/awesome
#ln -s `pwd`/nitrogen ~/.config/nitrogen
#ln -s `pwd`/xmodmap ~/.xmodmap
#ln -s `pwd`/profile ~/.profile
#ln -s `pwd`/Xdefaults ~/.Xdefaults
#ln -s `pwd`/clipboard.urxvt /usr/lib/urxvt/perl/clipboard
####################

ln -s `pwd`/inputrc ~/.inputrc
ln -s `pwd`/init.el ~/.emacs
ln -s `pwd`/tmux.conf ~/.tmux.conf
ln -s `pwd`/weechat ~/.weechat
ln -s `pwd`/.zshrc ~/.zshrc


### VIM CONFIG ###
curl -Lo- https://bit.ly/janus-bootstrap | bash
cd ~/.vim/janus/vim/tools
git clone https://github.com/bling/vim-airline.git
cd -
ln -s `pwd`/.vimrc ~/.vimrc
##################
