### Makefile for bin
#

DST = $(HOME)/bin
REL = Honu/$(notdir $(shell pwd))

.PHONY: install report-vars

### Install
#	We used to make a symlink here from the user's home directory, if ~/bin
#	didn't exist.  It makes more sense to make ~/bin the place where we put
#	actual binaries, to keep them out of the Honu and Mathilda trees.

install::
	@if [ -L $(DST) ]; then			\
		echo "~/bin is a symlink; replacing it with a real directory.";	\
		rm $(DST);							\
	fi
	mkdir -p $(DST);

#	We don't nee to make any links to this directory; just put it in $PATH

### If we're using the MakeStuff package, chain in its Makefile
#	See ../Makefile for more information.
#
CHAIN = $(wildcard ../../MakeStuff/Makefile)
include $(CHAIN)

report-vars::
	@echo REL = $(REL)
	@echo DST = $(DST)
	@echo CHAIN=$(CHAIN)
