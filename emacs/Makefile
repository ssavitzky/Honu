### makefile for Honu/emacs
#

### Prevent make push from trying to push ljupdate

GITDIRS :=

.PHONY: install

### Install target
#
# install:: | ljupdate
install:: | html-helper-mode
install:: | $(HOME)/emacs

$(HOME)/emacs: | $(HOME)/Honu
	cd $(HOME); ln -sf Honu/emacs .

# This used to be shipped with Ubuntu, but went missing in 2015 or so.
html-helper-mode:
	git clone  git@github.com:ssavitzky/html-helper-mode.git

### If we're using the MakeStuff package, chain in its Makefile
#	See ../Makefile for more information.
#
CHAIN = $(wildcard ../../MakeStuff/Makefile)
include $(CHAIN)

report-vars::
	@echo CHAIN=$(CHAIN)
