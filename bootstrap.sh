#!/bin/sh
# Bootstrap script for my home directory:  Config and Tools
# This will work on systems with X and a fair amount of space; for a
# tiny system like a Raspberry Pi you'd need to be selective about
# which packages you install.

INSTALL_PKGS=true	# set false if you don't want to install packages

### Preliminaries:

if [ root = `whoami` ]; then
    echo "Trying to run this as root will lead to madness."
    exit 1
fi
if [ -z $SSH_AUTH_SOCK ]; then
    echo "Trying to run this without ssh-add will lead to frustration."
    exit 1
fi
cd $HOME

if $INSTALL_PKGS; then
touch .sudo_as_admin_successful # seen it.

### Packages:

# Required packages: if we don't get these, all is lost.
REQUIRED="git curl wget make rsync openssh-client openssh-server"

# Life is difficult without these, but it's possible.
ALMOST_ESSENTIAL="xttitle mailutils memtest86+ zile ntp"

# Comment this out if you want a headless system without X apps.  Only
# do that if only if you don't have space for the X libraries.  It is
# not necessary to have an X server -- X works perfectly well over ssh.
# You'll get into trouble later, because a lot of X stuff gets installed.
X="emacs git-doc git.el xdu xbase-clients gitk git-gui xterm"

sudo apt-get install $REQUIRED $ALMOST_ESSENTIAL $X

if [ -z "$X" ]; then
    sudo apt-get install emacs-nox
fi

fi # INSTALL_PKGS

### ssh setup:

# ensure that shared ssh connections work.  This not only speeds up the
# git cloning, but keeps dreamhost's limit on ssh connections in quick
# succession from kicking in.  Not necessary if your .ssh/config is
# different from my usual one.
mkdir -p .ssh/controlmasters

### Now that we have git, we can fetch the repositories we need:

# Prefix for the git repos
# TODO:  make the main one github, with savitzky.net as an alternate.
REPO=savitzky@savitzky.net:git/users/steve

# destination for the fetched repos.
DEST=vv/users/steve

mkdir -p $DEST

# fetch the repositories we need.
(cd $DEST;
 for r in Config MakeStuff; do
     if [ ! -d $$r ]; then git clone $REPO/$r.git; fi
 done
)

if $INSTALL_PKGS; then
    # TODO:  propagate X/NOX to select packages to install.
    (cd $DEST/Config; make install-pkgs)
fi

### Install the dotfiles and other goodies.
#   Note that not all of it will be useful or even usable without the
#   packages, but what's there will give you a comfortable $HOME.
(cd $DEST/Config; make install)

echo Behold the power of this fully armed and operational workstation.
echo Remember to set up both gnome and xfce power managers.
