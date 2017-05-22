brew cask install slack
brew cask install java
brew cask install tunnelblick
brew cask install caffeine
brew cask install wireshark
brew cask install spotify

brew install emacs --with-cocoa --with-gnutls
brew install npm
brew install ag
brew install golang
brew install vagrant
brew install htop
brew install bazel
brew install awscli
brew install watch
brew install jq
brew install coreutils
brew install protobuf
brew install nmap
brew install qcachegrind
brew install graphviz

defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false
defaults write NSGlobalDomain KeyRepeat -int 1
defaults write NSGlobalDomain InitialKeyRepeat -int 10
defaults write NSGlobalDomain com.apple.swipescrolldirection -int 0

go get -u github.com/dougm/goflymake
go get -u github.com/afking/gazel/gazel
go get -u github.com/rogpeppe/godef
go get -u golang.org/x/tools/cmd/guru

wget https://download.docker.com/mac/stable/Docker.dmg; open Docker.dmg

npm install jshint

sudo easy_install pip
sudo pip install jedi==0.8.1
sudo pip install epc==0.0.4
sudo pip install pylint
