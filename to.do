= to.do: =

o Some dotfiles, e.g. .gitconfig, really need to be modified by a script rather than
  simply symlinked.  That's because things like username and email address change from one
  system to another.  So use a file full of, e.g., git config --global ...  An alternative
  would be to include them.
  
o regular dotfiles should start with "_" rather than ".", to make them visible.

1111We
  @ https://github.com/blipvert/ljupdate/commits/master marginally more up to date
    o update emacs/depends.make

==============================================================================
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

==============================================================================
Local Variables:
    fill-column:90
End:

