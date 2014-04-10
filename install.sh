#! /bin/sh
mkdir -p ~/src
cd ~/src
git clone git@github.com:zsh-users/zaw.git
git clone git@github.com:zsh-users/zsh-history-substring-search.git

cd ~
git clone git://github.com/robbyrussell/oh-my-zsh.git

#git clone https://github.com/pdf/ubuntu-mono-powerline-ttf.git ~/.fonts/ubuntu-mono-powerline-ttf
#fc-cache -vf

#sudo ln -s `pwd`/irssi ~/.irssi
#sudo ln -s `pwd`/fluxbox ~/.fluxbox
#sudo ln -s `pwd`/awesome ~/.config/awesome
#sudo ln -s `pwd`/nitrogen ~/.config/nitrogen
#sudo ln -s `pwd`/vimrc ~/.vimrc
#sudo ln -s `pwd`/init.el ~/.emacs
#sudo ln -s `pwd`/inputrc ~/.inputrc
ln -s `pwd`/tmux.conf ~/.tmux.conf
#sudo ln -s `pwd`/xmodmap ~/.xmodmap
#sudo ln -s `pwd`/profile ~/.profile
#sudo ln -s `pwd`/Xdefaults ~/.Xdefaults
ln -s `pwd`/weechat ~/.weechat
ln -s `pwd`/.zshrc ~/.zshrc
ln -s `pwd`/.vimrc ~/.vimrc
#sudo ln -s `pwd`/clipboard.urxvt /usr/lib/urxvt/perl/clipboard


curl -Lo- https://bit.ly/janus-bootstrap | bash
