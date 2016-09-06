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
	cd setup; for f in *pkgs; do ./$$f; done

# Link Honu into home directory.
$(HOME)/Honu:
	pwd=`pwd`; cd $(HOME); ln -s $$pwd .

### If we're using the Tools package, chain in its Makefile
#	This is optional -- it doesn't affect basic functionality -- but it
#	brings in a lot of useful extras like "make push", recursive "make all",
#	and so on.  Note that include does the right thing if the file list is
#	empty, so we don't have to test for that.
#
#	Normally Makefile is a symlink, and local dependencies go into
#	depends.make; I decided to try something different this time.
#
CHAIN = $(firstword $(wildcard ../MakeStuff/Makefile ../Tools/Makefile))
include $(CHAIN)

### report-vars
#	report-vars is also defined in the Tools package, so you can use it to
#	see whether Tools/Makefile is properly chained in.
report-vars::
	@echo makeable= $(makeable)
	@echo INSTALL_DIRS= $(INSTALL_DIRS)
	@echo CHAIN=$(CHAIN)
