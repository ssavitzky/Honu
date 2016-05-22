#!/bin/sh
# Bootstrap script for Config and Tools.

if [ root = `whoami` ]; then
    echo "Trying to run this as root will lead to madness."
    exit 1
fi
if [ -z $SSH_AUTH_SOCK ]; then
    echo "Trying to run this without ssh-add will lead to frustration."
fi

# Required packages: if we don't get these, all is lost.
REQUIRED="git gitk git-gui curl wget make rsync openssh-client"

# Packages with names that may require updating from time to time
MAY_NEED_UPDATE="emacs24"

# Highly recommended packages
HIGHLY_RECOMMENDED="git-doc git.el"

sudo apt-get install $REQUIRED $MAY_NEED_UPDATE $HIGHLY_RECOMMENDED

### Now that we have git, we can fetch the repos we need:

# Prefix for the git repos
REPO=savitzky@savitzky.net:vv/git/users/steve

# destination for the fetched repos.
DEST=vv/users/steve

mkdir -p $DEST

# fetch the repositories we need.
(cd $DEST; for r in Config Tools; do
	       if [ ! -d $$r ]; then git clone $REPO/$r; fi
	   done)

# Install the dotfiles and other goodies.
(cd $DEST/steve/Config; make install-pkgs install)

echo Welcome to this fully-armed and operational workstation.
