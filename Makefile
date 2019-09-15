### Honu/Makefile
#
# Targets:
#	install		Install configuration files
#	install-pkgs	Install packages -- calls sudo as needed
#

# Which subdirectories have an install target?
makeable := $(shell for f in *; do [ -e $$f/Makefile ] && echo $$f; done)
INSTALL_DIRS := $(shell for f in $(makeable); do grep -sq install:: $$f/Makefile	\
						 && echo $$f; done)

###

.PHONY: install install-pkgs report-vars

install:: | $(HOME)/Honu

install::
	for d in $(INSTALL_DIRS); do (cd $$d; $(MAKE) install) done

install-pkgs::
	cd setup; make install-pkgs

# Link Honu into home directory.
$(HOME)/Honu:
	pwd=`pwd`; cd $(HOME); ln -s $$pwd .

### If we're using the MakeStuff package, chain in its Makefile
#	This is optional -- it doesn't affect basic functionality -- but it
#	brings in a lot of useful extras like "make push", recursive "make all",
#	and so on.  Note that include does the right thing if the file list is
#	empty, so we don't have to test for that.
#
#	Normally Makefile is a symlink to Makestuff/Makefile, and local
#	dependencies go into depends.make.  We do it differently here because
#	we want Honu to be as stand-alone as possible, in case you want to
#	simply clone it rather than bootstrap it.
#
CHAIN = $(wildcard ../MakeStuff/Makefile)
include $(CHAIN)

### report-vars
#	report-vars is also defined in the MakeStuff package, so you can use it to
#	see whether MakeStuff/Makefile is properly chained in.  It's also a very
#	handy way to see whether your make variables are defined properly.
report-vars::
	@echo makeable= $(makeable)
	@echo INSTALL_DIRS= $(INSTALL_DIRS)
	@echo CHAIN=$(CHAIN)
