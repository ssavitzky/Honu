### Makefile for Honu/opt
#

### clearing GITDIRS keeps make push from trying to push stuff from github
GITDIRS :=

### Targets:
OPT := dzen eless charm ljupdate my-go mustache gojekyll liquid my-ruby jekyll

### directories.  HERE is this directory relative to LOCAL_BIN and LOCAL_EMACS
HERE = ../../opt
LOCAL_BIN 	 = ../local/bin
LOCAL_EMACS 	 = ../local/emacs

.PHONY: install fetch 

help::
	@echo Honu/opt: make one or more of the following:
	@echo $(OPT)

### Fetch code from github
fetch:: | $(OPT)

### Install target
#
install::
	@echo The opt directory contains optional software, mostly from github.
	@echo The various targets apply local patches, in ./Patches, if necessary,
	@echo the executables are built locally, and symlinked into Honu/bin.
	@echo Targets: fetch "(all)", clean, and the individual targets:
	@echo "        " $(OPT)

$(LOCAL_BIN) $(LOCAL_EMACS):
	mkdir -p $@

### Cleanup
#
clean::
	rm -rf $(OPT)

### dzen2 is one of the status bars we use with xmonad; it's the simplest to compile.
#       The version shipped with Ubuntu 16.04 and later is recent enough to use with
#	our xmonad configuration.
dzen:	| $(LOCAL_BIN)
	[ -d $@ ] || git clone git@github.com:robm/dzen
	cd $@; git am ../Patches/dzen*.patch
	cd $@; $(MAKE)
	cd $(LOCAL_BIN); ln -s $(HERE)/$@/$@ .

### eless is something like less except that it uses emacs in view mode
#	It has a build.el, but it's not actually required because all of its
#	build products are in the repository.
eless:	| $(LOCAL_BIN)
	[ -d $@ ] || git clone git@github.com:kaushalmodi/eless.git
	cd $(LOCAL_BIN); ln -s $(HERE)/$@/$@ .

### charm is a command-line/terminal LJ client.
#	charm is written in python2 and poorly maintained (pretty much the same thing)

/usr/bin/python2:
	$(error Python2 required.  Install with 'apt install python2')

charm:	| $(LOCAL_BIN) /usr/bin/python2
	[ -d $@ ] || git clone git@github.com:ssavitzky/charm.git
	cd $(LOCAL_BIN); ln -s $(HERE)/$@/$@ .

### lj-update is a (currently broken) LJ posting mode for emacs.
#	It was originally  git://github.com/hober/ljupdate.git, but the fork is
#	more recent.  It doesn't work with sites (like DW) that redirect to https.
ljupdate: | $(LOCAL_EMACS)
	$(warning ljupdate mode is deprecated until it gets fixed, if it ever does)
	git clone git@github.com:blipvert/ljupdate.git
	(cd ljupdate; yes | make fetch compile)
	cd $(LOCAL_EMACS); ln -sf $(HERE)/ljupdate .

### Programs that depend on Go:

# my-go sets up the initial go environment:
my-go:	

### mustache-cli is a command-line mustache templating engine written in golang
#	The executable is called mustache, and lives in $(LOCAL_BIN)
#	It's really not clear whether to rewrite this using `go get` etc,
#	or to rewrite liquid and gojekyll to look more like mustache.
#	This set of recipes for mustache has the advantage of putting
#	the executable under Honu/opt/
.PHONY: mustache
mustache: $(LOCAL_BIN)/mustache

$(LOCAL_BIN)/mustache:  mustache-cli/mustache | $(LOCAL_BIN)
	cd $(LOCAL_BIN); ln -s $(HERE)/$< .

mustache-cli/mustache:	 mustache-cli
	cd $<; $(MAKE)

mustache-cli:
	git clone git@github.com:quantumew/mustache-cli.git

# liquid is a templating engine for liquid templates, written in golang
liquid: REPO = github.com/osteele
liquid: | $(LOCAL_BIN)
	if [ ! -d $(HOME)/go/src/$$REPO/$@ ]; then	\
	   go get $(REPO)/$@;				\
        else						\
	   go get -u $(REPO)/$@;			\
	fi
	go install $(REPO)/liquid/cmd/liquid
	cd $(LOCAL_BIN); ln -rsf $(HOME)/go/bin/$@ .

# gojekyll is a partial clone of the Jekyll static site builder, in golang
gojekyll:  REPO = github.com/osteele
gojekyll:	| $(LOCAL_BIN)
	if [ ! -d $(HOME)/go/src/$(REPO)/$@ ]; then	\
	   go get $(REPO)/gojekyll;			\
        else						\
	   for f in gojekyll liquid tuesday; do		\
		go get -u $(REPO)/$$f; done;		\
	   go install $(REPO)/$@;			\
	fi
	cd $(LOCAL_BIN); ln -rsf $(HOME)/go/bin/$@ .

### Programs that depend on Ruby

# my-ruby sets up the Ruby development environment
my-ruby:

### If we're using the MakeStuff package, chain in its Makefile
#	See ../Makefile for more information.
#
CHAIN = $(wildcard ../../MakeStuff/Makefile)
include $(CHAIN)

report-vars::
	@echo " OPT="$(OPT)
	@echo " CHAIN="$(CHAIN)
