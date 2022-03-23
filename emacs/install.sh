#!/usr/bin/env bash

set -e
cd $(dirname "$0")

if [ -f /etc/lsb-release ]; then
  echo "Running Doom-Emacs installation for Debian based system"

  echo "Installing dependencies"
  sudo apt-get -y update
  sudo apt-get -y install ripgrep fd-find snapd

  echo "Installing latest version of Emacs"
  sudo snap install emacs --classic

  echo "Installing latest version of Doom-Emacs"
  git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
  ~/.emacs.d/bin/doom install

  echo "Installing config"
  mkdir -p ~/.doom.d
  cp -r ../doom-emacs/* ~/.doom.d

  echo "Installing Doom-Emacs packages"
  ~/.emacs.d/bin/doom sync
fi

echo "Done"
