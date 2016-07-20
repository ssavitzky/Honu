= to.do: =
	
  o bloggish directories should have a "make wc" target that subtracts the word count of
    the template file.  Or, better, use sed to filter out tags.

  o the ability to make ad-hoc workspace labels might help.  Put the list in a file that
    can be read in either xmonad or the bottom-bars script.  Possibly a top dbar with
    vertical menu. (0719)
    
========================================================================================>
Work Log:
========

2015:
=====

1111We
  @ <a href="https://en.wikibooks.org/wiki/OpenSSH/Cookbook/Multiplexing"
    >OpenSSH/Cookbook/Multiplexing - Wikibooks</a>
    Makes ssh much faster, and circumvents dreamhost's connection limits.
    > mkdir ~/.ssh/controlmasters; chmod 700 ~/.ssh/controlmasters
  * Add the following to .ssh/config, in the global section at the top:
      ControlPath ~/.ssh/controlmasters/%r@%h:%p
      ControlPersist 10m
      ControlMaster auto
  @ https://github.com/blipvert/ljupdate/commits/master marginally more up to date
    * update emacs/depends.make

2016:
=====

0214Su
  * much hacking in gnome-setup and font setup over the last week or so.

0215Mo
  * added adobe-source-code-pro font, tell emacs to use system font.

... Really ought to copy the config-related stuff out of starport/Admin/admin.to.do

0626
  ~ alltray looked useful - lets you put an icon for any program into the tray.
    It starts the program if it's not already running, but fails in xmonad because it's
    waiting for the program to be iconified.
  * gsimplecal, fbpanel, and probably others have subdirectories in .config
    probably best is to make another directory called dotconfig, parallel to dotfiles
 
0627Mo
  * made the Makefiles independendent of Tools/Makefile.

0704Mo
  * git subtree add --prefix bin ../bin master
  * The idea is that if there's a pre-existing ~/bin, we can symlink into it.

0705Tu
  * regular dotfiles should start with "_" rather than ".", to make them visible.
    -> move dotfiles/.* -> dotfiles/_*; update dotfiles/Makefile

0716Sa
  . Some dotfiles may need to be modified by a script rather than simply symlinked.
    That's because things like username and email address change from one system to
    another.  In most cases, though, one can use include files generated by make install.

0719Tu

  * When site-dependent parameters can be in separate files (include files/modules).
    the various install targets can initialize them from templates.  A good place to start
    would be .xmonad/Local.hs.  Could also do this for .bashrc; a conditional include
    would make it unnecessary to generate the file if it's not needed. (0716)
    : for gitconfig - note that the file doesn't need to exist
      [include]
	  path = /path/to/file
	  
==============================================================================
Local Variables:
    fill-column:90
End:

