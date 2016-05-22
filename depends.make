### depends.make for Config
#
# Targets:
#	install
#

INSTALL_DIRS := dotfiles emacs 

###

.PHONY: install fetch

install:: | $(HOME)/Config

install::
	for d in $(INSTALL_DIRS); do (cd $$d; $(MAKE) install) done

install::			# This should eventually be moved to ./setup
	cd setup; for f in *setup*; do ./$$f; done

install-pkgs::
	cd setup; for f in *pkgs; do ./$$f; done

# Link Config into home directory.
$(HOME)/Config:
	pwd=`pwd`; cd $(HOME); ln -s $$pwd .
