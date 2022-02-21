#!/bin/bash
# Upgrade everything after fresh-install
dnf upgrade -y

# Install the usual apps
dnf install vim zsh terminator -y
dnf install nitrogen compton -y
dnf install git emacs ripgrep -y
dnf install fd-find -y

# Install xmonad and related apps
dnf install xmonad dmenu xmobar xscreensaver stalonetray -y
