= to.do: =

o Some dotfiles, e.g. .gitconfig, really need to be modified by a script rather than
  simply symlinked.  That's because things like username and email address change from one
  system to another.  So use a file full of, e.g., git config --global ...  An alternative
  would be to include them.
  
o regular dotfiles should start with "_" rather than ".", to make them visible.

========================================================================================>
Work Log:
========

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

2016

0214Su
  * much hacking in gnome-setup and font setup over the last week or so.

0215Mo
  * added adobe-source-code-pro font, tell emacs to use system font.

... Really ought to copy the config-related stuff out of starport/Admin/admin.to.do

0626
  ~ alltray lookeed useful - lets you put an icon for any program into the tray.
    It starts the program if it's not already running, but fails in xmonad because it's
    waiting for the program to be iconified.
  * gsimplecal, fbpanel, and probably others have subdirectories in .config
    probably best is to make another directory called dotconfig, parallel to dotfiles
 
0627Mo
  * made the Makefiles independendent of Tools/Makefile.

==============================================================================
Local Variables:
    fill-column:90
End:

