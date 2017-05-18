### Makefile for opt
#

### Prevent make push from trying to push stuff from github

GITDIRS :=
OPT := dzen eless
INSTALL_TARGETS :=  $(addprefix install-, $(OPT))
LOCAL_BIN = ../local/bin

.PHONY: install fetch $(addprefix install-, $(OPT))

### Fetch code from github
fetch:: | $(OPT)

### Install target
#
install::
	@echo The opt directory contains optional software, mostly from github.
	@echo Local patches, in ./Patches, are applied automatically on fetch.
	@echo Targets: fetch "(all)", clean, and the individual install targets:
	@echo "        " $(INSTALL_TARGETS)

$(LOCAL_BIN):
	mkdir $@

### Cleanup
#
clean::
	rm -rf $(OPT)

# dzen2 is one of the status bars we use with xmonad; it's the simplest to compile.
dzen:
	git clone git@github.com:robm/dzen
	cd $@; git am ../Patches/dzen*.patch
	cd $@; $(MAKE)

install-dzen:: | dzen
	@echo NOTE:  dzen installs into /usr/local by default
	cd dzen; make install

# Note that eless doesn't have a makefile.
#	It has a build.el, but it's not actually required because all of its
#	build products are in the repository.
eless:
	git clone git@github.com:kaushalmodi/eless.git

install-eless: $(LOCAL_BIN)/eless

$(LOCAL_BIN)/eless: | eless eless/eless $(LOCAL_BIN)
	cd $(LOCAL_BIN); ln -s ../../opt/eless/eless .

### If we're using the MakeStuff package, chain in its Makefile
#	See ../Makefile for more information.
#
CHAIN = $(wildcard ../../MakeStuff/Makefile)
include $(CHAIN)

report-vars::
	@echo OPT = $(OPT)
	@echo INSTALL_TARGETS = $(INSTALL_TARGETS)
	@echo CHAIN=$(CHAIN)
