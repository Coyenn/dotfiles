#!/usr/bin/env bash

set -e
cd "$(dirname "$0")"

if [ "$(uname)" == "Darwin" ]; then
  echo "Running Doom Emacs installation for OSX based system"
  echo "Adding brew cask"
  brew cask install emacs #is the preferred from emacs wiki

  echo "Installing from cask"
  brew install --cask emacs #using new cask syntax

  echo "Cloning Doom Emacs"
  git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d

  echo "Running doom install"
  ~/.emacs.d/bin/doom install

  echo "Installing config"
  mkdir -p ~/.doom.d
  cp -r ../doom-emacs/* ~/.doom.d

  echo "Installing Doom Emacs packages"
  ~/.emacs.d/bin/doom sync
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
  if [ -f /etc/lsb-release ]; then
    echo "Running Doom-Emacs installation for Debian based system"

    echo "Installing dependencies"
    sudo apt-get -y update
    sudo apt-get -y install ripgrep fd-find snapd

    echo "Installing latest version of Emacs"
    sudo snap install emacs --classic

    echo "Installing latest version of Doom Emacs"
    git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
    ~/.emacs.d/bin/doom install

    echo "Installing config"
    mkdir -p ~/.doom.d
    cp -r ../doom-emacs/* ~/.doom.d

    echo "Installing Doom Emacs packages"
    ~/.emacs.d/bin/doom sync
  else
    echo "Only Debian based Linux systems are currently supported"
  fi
elif [ "$(expr substr $(uname -s) 1 10)" == "MINGW32_NT" ]; then
  echo "Windows is currently not supported"
elif [ "$(expr substr $(uname -s) 1 10)" == "MINGW64_NT" ]; then
  echo "Windows is currently not supported"
fi

echo "Done"
