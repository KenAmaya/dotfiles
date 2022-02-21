#!/bin/bash

# Install Flatpak
flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo

# Install Flatpak apps
flatpak install flathub com.discordapp.Discord -y
flatpak install flathub org.videolan.VLC -y
flatpak install flathub com.github.micahflee.torbrowser-launcher -y
flatpak install flathub com.valvesoftware.Steam -y

# Install oh-my-zsh
sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# Install oh-my-zsh settings
git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/themes/powerlevel10k

# Install Homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
echo 'eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"' >> /home/Ken/.bash_profile 
eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
path+=/home/linuxbrew/.linuxbrew/bin/brew
brew update --force --quiet
chmod -R go-w eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
yum groupinstall 'Development Tools' -y

# Install apps from brew 
brew install gcc
brew install gh
## Install node.js
brew install node

# Install pipenv
pip3 install --user pipenv
path+=/home/$USER/.local/bin


# Install git config username and email
git config --global user.name KenAmaya
git config --global user.email ken.amaya@protonmail.ch

# Install Doom Emacs
git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/doom install

