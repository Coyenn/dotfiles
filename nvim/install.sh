#!/usr/bin/env bash

set -e
cd $(dirname "$0")

if [ -f /etc/lsb-release ]; then
  echo "Running Neovim installation for Debian based system"

  echo "Installing dependencies"
  sudo apt-get -y update
  sudo apt-get -y install nodejs npm
  sudo npm install -g yarn@latest

  echo "Installing latest version of neovim"
  sudo add-apt-repository ppa:neovim-ppa/stable -y
  sudo apt-get update
  sudo apt-get install neovim

  echo "Installing config"
  mkdir -p ~/.config/nvim
  cp ./init.vim ~/.config/nvim
fi

echo "Done"
