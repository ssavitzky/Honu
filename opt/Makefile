### depends.make for Config/opt
#

### Prevent make push from trying to push stuff from github

GITDIRS :=
OPT := dzen
INSTALL_TARGETS :=  $(addprefix install-, $(OPT))

.PHONY: install fetch $(addprefix install-, $(OPT))

### Fetch code from github
fetch:: | $(OPT)

### Install target
#
install::
	@echo The opt directory contains optional software, mostly from github
	@echo Individual install-xxx targets can be used; it may be necessary
	@echo to do some configuration first.  When in doubt, make fetch
	@echo targets: $(INSTALL_TARGETS)


# dzen2 is one of the status bars we use with xmonad; it's the simplest to compile.
dzen:
	git clone git@github.com:robm/dzen
	@echo dzen may have to be configured for xinerama.  See README


install-dzen:: | dzen
	@echo NOTE:  dzen installs into /usr/local by default
	cd dzen; make install

### If we're using the Tools package, chain in its Makefile
#	See ../Makefile for more information about Tools.
#
CHAIN = $(wildcard ../../Tools/Makefile)
ifneq ($(CHAIN),)
    include ../../Tools/Makefile
endif

report-vars::
	@echo OPT = $(OPT)
	@echo INSTALL_TARGETS = $(INSTALL_TARGETS)
	@echo CHAIN=$(CHAIN)
