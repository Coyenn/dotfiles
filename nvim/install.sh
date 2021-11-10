#!/bin/bash

echo "Installing dependencies"
sudo apt-get -y install nodejs npm
sudo sudo npm install -g yarn@latest

echo "Installing latest version of neovim"
sudo apt remove neovim -y && \
sudo add-apt-repository ppa:neovim-ppa/stable  && \
sudo apt-get update && \
sudo apt-get install neovim

echo "Installing config"
mkdir ~/.config/nvim
cp ,/init.vim ~/.config/nvim

echo "Done"
