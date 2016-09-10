#!/bin/sh
# Bootstrap script for Honu.
# Installs essential Debian/Ubuntu packages if it can, then clones
# Honu and MakeStuff, and runs make in Honu.

# The bootstrap process is controlled by the following environment variables:
# INSTALL_PKGS=true                     -- set to false if you're not in the sudo group
# HAVE_X=true                           -- set to false if $DISPLAY is undefined
# REPOS=https://github.com/ssavitzky    -- base for git repositories

# HAVE_X will work on systems with X and a fair amount of space; for a
# tiny system like a Raspberry Pi you'd want to export HAVE_X=false and
# be selective about which packages you install.

### Preliminaries:

if [ root = `whoami` ]; then
    echo "Honu creates many files and directories in your home directory."
    echo "If you are running as root, you won't be able to edit them."
    echo "Giving up."
    exit 1
fi

# Set sensible defaults for INSTALL_PKGS and HAVE_X

if [ -z $INSTALL_PKGS ]; then
    if groups | grep -q sudo; then
	INSTALL_PKGS=true	# set false if you don't want to install packages
    else
	echo "You don't appear to be in the sudo group, so I won't try to install packages."
	INSTALL_PKGS=false
    fi
fi

if [ -z $HAVE_X ]; then
    if [ -z $DISPLAY ]; then
	HAVE_X=false
    else
	HAVE_X=true
    fi
fi

if $INSTALL_PKGS; then		### Install packages.

    # Required packages: if we don't get these, all is lost.
    REQUIRED="git curl wget make rsync openssh-client openssh-server"

    # Life is difficult without these, but it's possible.
    ALMOST_ESSENTIAL="mailutils memtest86+ zile ntp"

    if $HAVE_X; then
	X_PKGS="emacs git-doc git.el xdu xbase-clients gitk git-gui xterm xttitle"
    else
	X_PKGS="emacs-nox"
    fi
    sudo apt-get -y install $REQUIRED $ALMOST_ESSENTIAL $X_PKGS

fi # INSTALL_PKGS

export INSTALL_PKGS HAVE_X

### Now that we have git, we can fetch the repositories we need:

# Prefix for the git repos
if [ -z "$REPOS" ]; then
    REPOS=https://github.com/ssavitzky
fi
# fetch the repositories we need.
for r in Honu MakeStuff; do
    if [ ! -d $$r ]; then git clone $REPOS/$r.git; fi
done

if $INSTALL_PKGS; then
    # TODO:  look at HAVE_X in the makefiles
    (cd Honu; make install-pkgs)
fi

### Install the dotfiles and other goodies.
#   Note that not all of it will be useful or even usable without the
#   packages, but what's there will give you a comfortable $HOME.
(cd Honu; make install)

echo You may need to set up both gnome and xfce power managers.
echo Home is wherever you carry your shell.  Welcome home, $USER.
