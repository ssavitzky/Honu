### Makefile for opt
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
	@echo The opt directory contains optional software, mostly from github.
	@echo Local patches, in ./patches, are applied automatically on fetch.
	@echo Targets: fetch "(all)", clean, and the individual install targets:
	@echo "        " $(INSTALL_TARGETS)

### Cleanup
#
clean::
	rm -rf $(OPT)

# dzen2 is one of the status bars we use with xmonad; it's the simplest to compile.
dzen:
	git clone git@github.com:robm/dzen
	cd dzen; git am ../patches/dzen*.patch
	cd dzen; $(MAKE)

install-dzen:: | dzen
	@echo NOTE:  dzen installs into /usr/local by default
	cd dzen; make install

### If we're using the MakeStuff package, chain in its Makefile
#	See ../Makefile for more information.
#
CHAIN = $(wildcard ../../MakeStuff/Makefile)
include $(CHAIN)

report-vars::
	@echo OPT = $(OPT)
	@echo INSTALL_TARGETS = $(INSTALL_TARGETS)
	@echo CHAIN=$(CHAIN)
