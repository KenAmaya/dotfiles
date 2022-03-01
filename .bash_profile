# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

alias lah="ls -lah"
alias lat="ls -laht"
alias discord="flatpak run com.discordapp.Discord"

alias dotgit="/usr/bin/git --git-dir=$HOME/.cfg_fedora/ --work-tree=$HOME"
# ^ The alias dotgit is regular git that interacts with our bare configuration repository

eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
. "$HOME/.cargo/env"
