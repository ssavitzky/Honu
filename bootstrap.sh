#!/bin/sh
# Bootstrap script for Config and Tools.

# Required packages: if we don't get these, all is lost.
REQUIRED="git gitk git-gui curl make rsync openssh-client"

# Packages with names that may require updating from time to time
MAY_NEED_UPDATE="emacs24"

# Highly recommended packages
HIGHLY_RECOMMENDED="git-doc git.el"

sudo install $REQUIRED $MAY_NEED_UPDATE $HIGHLY_RECOMMENDED


### Now that we have git, we can fetch the repos we need:

# Prefix for the git repos
REPO=savitzky@savitzky.net:vv/git/users/steve

# destination for the fetched repos.
DEST=vv/users/steve

mkdir -p $DEST

# fetch the repositories we need.
(cd $DEST; for r in Config Tools; do git clone $REPO/$r; done)

# Install the dotfiles and other goodies.
(cd $DEST/steve/Config; make install)

echo Please enjoy.
