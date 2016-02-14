#!/bin/bash
# Install symlinks to dotfiles in home directory.

# First make sure that Config is in $HOME
if [ ! -d $HOME/Config/dotfiles ]; then
    pwd=`/bin/pwd`
    config=`dirname $pwd`
    echo "linking Config->$config"
    (cd $HOME; ln -s $config .)
fi

# Now link all the dotfiles.
# Note that the .[^.]* construct doesn't work in /bin/sh.  Blame POSIX.
for f in .[^.]*; do
    echo linking "$f->Config/dotfiles/$f"
    (cd $HOME; ln -sf Config/dotfiles/$f .)
done

# Finally, clean up a broken symlink to .gitignore, if any.
# Eventually, this should probably be a loop over all dotfiles.
if [ -L ~/.gitignore -a ! -e ~/.gitignore ]; then
    echo removing broken symlink .gitignore
    rm ~/.gitignore
fi

# We may have installed or changed .fonts, so update the cache
fc-cache ~/.fonts/
