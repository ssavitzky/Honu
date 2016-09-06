### Makefile for dotfiles
#

DST = $(HOME)
REL = Honu/dotfiles
CFG = $(shell dirname `/bin/pwd`)

SRCFILES := $(wildcard _*)
DOTFILES := $(shell for f in $(SRCFILES); do echo $$f| sed -e 's/_/./'; done)
DSTFILES := $(addprefix $(HOME)/, $(DOTFILES))
# These are the originals we'll have to move out of the way:
NONLINKS := $(shell for f in $(DSTFILES); do [ -L $$f ] || [ ! -e  $$f ] || echo $$f; done)

# Here's where we move any original (non-dot) files we find.
ORIGINALS = ../local/originals

.PHONY: install cleanup report-vars

### Install dotfiles in $(HOME).
#
install::	| $(HOME)/$(REL)
	@[ -z "$(NONLINKS)" ] || echo moving $(NONLINKS) to $(ORIGINALS)
	@for f in $(NONLINKS); do mkdir $(ORIGINALS); mv $$f $(ORIGINALS); done
	@for f in $(SRCFILES); do \
		(cd $(DST) 						\
		; ln -nsf $(REL)/$$f `echo $$f| sed -e 's/_/./'`	\
		) done

# We may have installed or changed .fonts, so update the cache
install::
	fc-cache ~/.fonts/

$(HOME)/$(REL):
	echo "linking Honu->$(CFG)"
	(cd $(HOME); ln -s $(CFG) .)

# Remove broken symlinks in $(HOME)
cleanup::
	@for f in $(HOME)/.*; do			\
	    if [ -L $$f -a ! -e $$f ]; then		\
	    	echo removing broken symlink $$f;	\
	    	rm $$f;					\
	    fi						\
	done

### If we're using the MakeStuff package, chain in its Makefile
CHAIN = $(firstword $(wildcard ../../MakeStuff/Makefile ../../../MakeStuff/Makefile)
include $(CHAIN)

report-vars::
	@echo CFG= $(CFG)
	@echo SRCFILES=$(SRCFILES)
	@echo NONLINKS=$(NONLINKS)
	@echo CHAIN=$(CHAIN)
