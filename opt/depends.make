### depends.make for Config/opt
#

### Prevent make push from trying to push stuff from github

GITDIRS :=

.PHONY: install fetch

### Fetch code from github
fetch:: | dzen

### Install target
#
install:: | fetch
	@echo This directory contains optional software, mostly from github
	@echo Individual install-xxx targets can be used; it may be necessary
	@echo to do some configuration first.


# dzen2 is one of the status bars we use with xmonad; it's the simplest to compile.
dzen:
	git clone git@github.com:robm/dzen
	@echo dzen may have to be configured for xinerama.  See README


install-dzen:: | dzen
	@echo NOTE:  dzen installs into /usr/local by default
	cd dzen; make install
