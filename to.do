= Honu: to.do: =
	
  o bloggish directories should have a "make wc" target that subtracts the word count of
    the template file.  Or, better, use sed to filter out tags.

  o bootstrap should check ~/.ssh to see whether to use savitzky.net or github.

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

0725Mo
  * color-coded workspace labels.  ws N makes an xclock with WS=N and the appropriate
    resistor color code.
  * the ability to make ad-hoc workspace labels might help.  Put the list in a file that
    can be read in either xmonad or the bottom-bars script.  Possibly a top dbar with
    vertical menu. (0719) -> add a second argument to ws.
  * Re-running ws N should grep ps for the appropriate color code and kill it.
    An alternative to xclock would be dzen -p.

0726Tu
  x use dzen for workspace labels?  Note that dzen is smaller, but only slightly.
    Problem is that it doesn't seem possible to have it shrinkwrap text.
  * separate widths for first and other top bars; make default 950; adjust ws

0827Sa
  * Allow MakeStuff as well as Tools.  Eventually it will be possible to drop Tools as
    an alternative.

0828Su
  ^ Config -> Honu.  The only real question is whether I want to make Honu a separate
    version, cleaned up and generalized for distribution. -> I don't think so.  It would
    be hard to keep them in sync.  The main me-specific things are the link pages in web,
    and some of the stuff in Archive, but there are also some of the dotfiles (e.g.
    .signature and .Xdefaults-*), stuff in the emacs config, and...  On second thought: I
    think I'd be a very uncomfortable about putting .mailrc, .gnus, and the bookmarks out
    there.  So there's going to have to be a rewrite anyway.  Those can go in -Personal-
  ^ Problematic stuff -- bookmarks, .mailrc, .gnus, etc. belongs in a separate git repo
    that's a subdirectory of local!  Can of course have dotfiles and dotconfig subdirs.
    Can have its own bootstrap, which would get the user-specific out of there, too.  In
    fact, call it local/local-Config or local/My-Honu or some such and derive it from the
    original, with all the old history.  Make a relatively clean start for Honu.

0829Mo
  o bootstrap and the first run of make should log messages, and print out the log on
    exit.

0904Su
  * clone from Config; clean up with bfg:
    78  alias bfg='java -jar ~/Downloads/bfg-1.12.13.jar'
    85  bfg --help
    87  git commit -m "deletions before bfg"
    88  bfg --delete-folders Archive
    91  bfg --delete-folders Archive
    92  bfg --delete-folders web
    94  bfg -D [._]mailrc
    95  bfg -D [._]gnus
    96  bfg -D [._]sig*
    97  bfg -D [hw].home.html
   107  git filter-branch --prune-empty
   110  git reflog expire --expire=now --all && git gc --prune=now --aggressive

0905Mo
  * Push to github.
  o .emacs:  generate, and load Honu/emacs/emacs.el and Honu/local/emacs.el
  o testing is needed:
    Set up a testing environment by setting USER and HOME, and putting a dummy sudo
    that simply records what it's supposed to do on $PATH.  Ought to have a dummy groups
    command as well. -> see MakeStuff for test framework.
  o Originals:  Should go into, e.g., local/orig  (ignore subdirectories of local)
  * Myrtle/Mathilda - install into the same directory as Honu and MakeStuff.
    symlink files and subdirs into Honu/local, or directly into $HOME if they don't
    overlap.
  o add scsitools to packages

0911Su
  * no links to bottom-bars, bottom-mobars
  o length on bottom-bars -- see ws for how to get this

0912Mo
  ^ At some point, we may want to move some of the settings most likely to be customized
    out into Myrtle/Mathilda.  That includes whether to pull down MakeStuff -- Honu should
    be completely stand-alone and not need much customization.
    
========================================================================================>|
Local Variables:
    fill-column:90
End:

