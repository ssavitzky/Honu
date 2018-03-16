### Makefile for opt
#

### Prevent make push from trying to push stuff from github

GITDIRS :=
OPT := dzen eless charm ljupdate mustache-cli
HERE = ../../opt
INSTALL_TARGETS :=  $(addprefix install-, $(OPT))
LOCAL_BIN 	 = ../local/bin
LOCAL_EMACS 	 = ../local/emacs

.PHONY: install fetch $(addprefix install-, $(OPT))

### Fetch code from github
fetch:: | $(OPT)

### Install target
#
install::
	@echo The opt directory contains optional software, mostly from github.
	@echo The various fetch targets apply local patches, in ./Patches, if necessary,
	@echo the executables are built locally, and symlinked into Honu/bin.
	@echo Targets: fetch "(all)", clean, and the individual fetch and install targets:
	@echo "        " $(OPT)
	@echo "        " $(INSTALL_TARGETS)
	@echo The install targets are only useful if you want to do a system-wide install.

$(LOCAL_BIN):
	mkdir -p $@

$(LOCAL_EMACS):
	mkdir -p $@

### Cleanup
#
clean::
	rm -rf $(OPT)

# dzen2 is one of the status bars we use with xmonad; it's the simplest to compile.
#       The version shipped with Ubuntu 16.04 and later is recent enough to use with
#	our xmonad configuration.
dzen:
	[ -d $@ ] || git clone git@github.com:robm/dzen
	cd $@; git am ../Patches/dzen*.patch
	cd $@; $(MAKE)
	cd $(LOCAL_BIN); ln -s $(HERE)/$@/$@ .

install-dzen:: | dzen
	@echo NOTE:  dzen installs into /usr/local/bin by default
	@echo It is not needed on Ubuntu 16.04 or later.
	cd dzen; make install

# Note that eless doesn't have a makefile.
#	It has a build.el, but it's not actually required because all of its
#	build products are in the repository.
eless:
	[ -d $@ ] || git clone git@github.com:kaushalmodi/eless.git
	cd $(LOCAL_BIN); ln -s $(HERE)/$@/$@ .

install-eless: $(LOCAL_BIN)/eless
	install eless/eless /usr/local/bin

# charm is a command-line/terminal LJ client.
charm:
	[ -d $@ ] || git clone git@github.com:ssavitzky/charm.git
	cd $(LOCAL_BIN); ln -s $(HERE)/$@/$@ .

install-charm: | charm
	@echo NOTE:  charm installs into /usr/local/bin by default
	cd charm; python setup.py install

# lj-update is a (currently broken) LJ posting mode for emacs.
#	It was originally  git://github.com/hober/ljupdate.git, but the fork is
#	more recent.  It doesn't work with sites (like DW) that redirect to https.
ljupdate:
	git clone git@github.com:blipvert/ljupdate.git
	(cd ljupdate; yes | make fetch compile)

install-ljupdate: | $(LOCAL_EMACS) ljupdate
	cd $(LOCAL_EMACS); ln -sf $(HERE)/ljupdate .

# mustache-cli is a command-line mustache templating engine written in go
mustache-cli:
	[ -d $@ ] || git clonegit@github.com:quantumew/mustache-cli.git
	cd $@; $(MAKE)
	cd $(LOCAL_BIN); ln -s $(HERE)/$@/mustache .

### If we're using the MakeStuff package, chain in its Makefile
#	See ../Makefile for more information.
#
CHAIN = $(wildcard ../../MakeStuff/Makefile)
include $(CHAIN)

report-vars::
	@echo OPT = $(OPT)
	@echo INSTALL_TARGETS = $(INSTALL_TARGETS)
	@echo CHAIN=$(CHAIN)
