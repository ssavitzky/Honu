### depends.make for Config/emacs
#

### Prevent make push from trying to push ljupdate

GITDIRS :=

.PHONY: install

### Install target
#
install:: | ljupdate
install:: | html-helper-mode
install:: | $(HOME)/Config
install:: | $(HOME)/emacs

# This will do for now.  Should really be in ..
$(HOME)/Config:
	cd ../dotfiles; $(MAKE) install

$(HOME)/emacs: | $(HOME)/Config
	cd $(HOME); ln -sf Config/emacs .

# was originally  git://github.com/hober/ljupdate.git, but the fork is
# more recent.
ljupdate:
	git clone git@github.com:blipvert/ljupdate.git
	(cd ljupdate; yes | make fetch compile)

# This used to be shipped with Ubuntu, but went missing in 2015 or so.
html-helper-mode:
	git clone  https://github.com/emacsattic/html-helper-mode
