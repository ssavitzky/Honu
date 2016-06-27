### Makefile for dotconfig
#

.PHONY: install

DST = $(HOME)/.config
REL = ../Config/dotconfig

#  Note that we're ignoring editor backup and .sh files.
CFGFILES = $(filter-out %.sh, $(wildcard [a-z]*[^~]))
FILES = $(shell for f in $(CFGFILES); do [ -d $$f ] || echo $$f; done)
DIRS  = $(shell for f in $(CFGFILES); do [ -d $$f ] && echo $$f; done)

.PHONY: install list

### Install plain files

install:: $(FILES)
	for f in $(FILES); do (cd $(DST); ln -sf $(REL)/$$f .) done

### Install directories
#
#   Things are complicated by the fact that programs with a directory in .config
#   expect it to be a real directory, not a symlink.  So we make directories and
#   symlink the contents.  Clean up editor backup files afterward, which takes
#   care of edits in both directories (and is much simpler)

install:: $(DIRS)
	for d in $(DIRS); \
	     do ( cd $(DST) ; mkdir -p $$d ; cd $$d  			\
		; ln -sf ../$(REL)/$$d/* . ; rm -f *~  			\
		) done

list:
	@echo CFGFILES=$(CFGFILES)
	@echo FILES = $(FILES)
	@echo DIRS = $(DIRS)
