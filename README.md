Honu
====
Because home is wherever:
-------------------------

> Where the wind takes us next year no turtle can tell<br>
> But we'll still be at home, come high water or hell,<br>
> Because home is wherever you carry your shell.<br>
> -- [Windward](http://steve.savitzky.net/Songs/windward/), by Stephen
> Savitzky, 2015

This project contains a set of configuration files, with Makefiles for setup
and installation, that allow you to set up a home directory exactly the way I
like it, with a single command. Unlike its predecessor, which was very
specific to me, it's meant to be reasonably general -- it expects to find its
repository on github, leaves your .mailrc and .signature files alone, and
preserves most of your old configuration in case you didn't really mean it.

On the other hand, it still sets up my favorite aliases, gnome configuration
options, emacs options, and xmonad configuration. Forking is not only
encouraged, it's practically mandatory if you want to stay sane.

That said, there are provisions for personal and machine-local customizations:
see the local directory.  I first started putting those in to accommodate
laptopand desktop systems with a wide range of screen sizes; it's gotten
somewhat more general since then.

There is also a sample customization package that provides a wrapper around
Honu, called (of course) Myrtle.  (I have my own, too; it's called Mathilda.
In case you were wondering, "Honu" is the Hawaiian word for the green
sea-turtle, and Mathilda is our name for the narrator of "Windward".)

Annotated Contents
------------------

### Files

`bootstrap.sh`

This is the shell script that runs the configuration process.  Just say:<br>
`wget -O - https://github.com/ssavitzky/Honu/raw/master/bootstrap.sh|bash`<br>
or something of the sort. You can also clone the repo and source it, but
half the fun is watching the magic unfold.

Honu will install itself in whatever directory you run the script in, and make
a symlink in your home directory if necessary.

`MIT-LICENSE.txt`

The license for this project.

`Makefile`

Once things get bootstrapped, the entire configuration process is run
via the Makefile. `make install` is the main target; eventually there
will be targets for partial installs, e.g. on tiny systems or systems
where I don't have `sudo` access.

`to.do`

My to-do list, of course. The format is trivial: an open circle
(lowercase "o") is something that's not started yet; a period is
something in progress, and a filled circle (asterisk) is finished. "\~"
indicates something I've decided not to do, and "?" indicates something
I'm dithering about. "&" indicates something I did that I wasn't
planning to do. Eventually finished items move to a "done" section.

### Directories

`bin/`

Programs. Mostly small shell scripts; I don't think there are any actual
binary files in there at the moment. Some are symlinks into other parts
of the tree, e.g. dotfiles/\_xmonad. No doubt many are useless at this
point; some date back to the days of usenet.

`dotconfig/`

Configuration files and directories that go into `~/.config/`. These are
symlinked so as not to conflict with the stuff already there.

`dotfiles/`

Configuration files that go into the home directory. They are symlinked
rather than being copied.

`emacs/`

Like it says on the tin: configuration files for emacs. This doesn't
include `.emacs`, which you'll find in `dotfiles`.

`local/`

Files that are expected to be edited on a per-system or per-user basis.
Initial versions are created by `make install`

`opt/`

The Makefile here lets you download and install various optional
programs from elsenet. (Right now the only one is `dzen2` -- older
releases of Debian and Ubuntu don't have one that's recent enough to
include all the features I want to use with `xmonad`.)

`setup/`

Scripts for configuring the things that can't (easily) be installed by
simply symlinking a dotfile. This includes a lot of Gnome options. There
are also files with a `-pkgs` suffix, that install programs using `apt`.

------------------------------------------------------------------------

**Copyright © Stephen R. Savitzky (HyperSpace Express)**

------------------------------------------------------------------------
