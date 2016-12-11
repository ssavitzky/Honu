### Makefile for bin
#

DST = $(HOME)/bin
REL = Honu/$(notdir $(shell pwd))

.PHONY: install report-vars


### Install
#	It's an open question whether we want to bother making a link in $(HOME) at all,
#	but it seems like a nice convenience, since most people expect to see a ~/bin.
install::
	@if [ -e $(DST) ] && [ ! -L $(DST) ]; then			\
		echo "~/bin exists: linking to local/honu_bin";		\
		(cd ../local; ln -snf ../bin honu_bin)			\
	else								\
		echo "Symlinking $(REL) to ~/bin";			\
		(cd $(HOME); ln -snf $(REL) .)				\
	fi

### If we're using the MakeStuff package, chain in its Makefile
#	See ../Makefile for more information.
#
CHAIN = $(wildcard ../../MakeStuff/Makefile)
include $(CHAIN)

report-vars::
	@echo REL = $(REL)
	@echo DST = $(DST)
	@echo CHAIN=$(CHAIN)
