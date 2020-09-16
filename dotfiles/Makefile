### Makefile for dotfiles
#

DST = $(HOME)
REL = Honu/dotfiles
CFG = $(shell dirname `/bin/pwd`)

# Here's where we move any original (non-dot) files we find.
ORIGINALS = $(DST)/.honu-originals
$(ORIGINALS):
	mkdir $@

### SRCFILES: dotfiles that can simply be replaced by symlinks are prefixed with _
SRCFILES := $(wildcard _*)
DOTFILES := $(shell for f in $(SRCFILES); do echo $$f| sed -e 's/_/./'; done)
DSTFILES := $(addprefix $(HOME)/, $(DOTFILES))
# These are the originals we'll have to move out of the way:
NONLINKS := $(shell for f in $(DSTFILES); do [ -L $$f ] || [ ! -e  $$f ] || echo $$f; done)

.PHONY: install cleanup report-vars

### Install dotfiles in $(HOME).
#
install::	| $(HOME)/$(REL) $(ORIGINALS)
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

### SRCDIRS:  directories that need to have their contents symlinked
#	(stripped of the leading =, so that they can be prefixed as needed)
SRCDIRS := $(subst =,,$(wildcard =*))

#	We start by moving the original (which might be either a directory or a
#	link -- we don't care -- into $(ORIGINALS) if it isn't there already.
#	We assume that there is only one level of subdirectories (e.g. =xmonad/lib)
#
install::  install_dirs
install_dirs::  | $(HOME)/$(REL) $(ORIGINALS)
	for d in $(SRCDIRS); do							\
	    if [ ! -e  $(ORIGINALS)/.$$d ] && [ -e $(DST)/.$$d ]; then 		\
		mv $(DST)/.$$d $(ORIGINALS);					\
	    else								\
		touch $(ORIGINALS)/.$$d;					\
	    fi;									\
	    mkdir -p $(DST)/.$$d; 						\
	    (cd =$$d; for f in *; do						\
		if [ -d $$f ]; then 						\
		    if [ ! -d $(DST)/.$$d/$$f ]; then rm -f $(DST)/.$$d/$$f; fi;	\
		    mkdir -p  $(DST)/.$$d/$$f;					\
		    for g in $$f/*; do						\
			echo ../../$(REL)/=$$d/$$g  "->" $(DST)/.$$d/$$f;	\
			ln -sf ../../$(REL)/=$$d/$$g $(DST)/.$$d/$$f;		\
		    done							\
		else								\
		    echo ../$(REL)/=$$d/$$f "->" $(DST)/.$$d;			\
		    ln -sf ../$(REL)/=$$d/$$f $(DST)/.$$d;			\
		fi								\
	    done);								\
	done

# .gtkrc-2.0 gets modified by style themes; in Gnome3 it has an "include .gtkrc-2.0.mine"
# in xfce it doesn't, but seems to incude .gtkrc-2.0.xfce (and nothing else).
# That explains the weird things that happened after I did an install on Mint.
# We can treat .gtkrc-2.0.mine as an ordinary dotfile, and append the include if it
# isn't already there.  Allow for other instances of the same problem.
#
# It's easy to imagine cases where this won't work; deal with those as needed,
# possibly by using sed to prepend the include statement.
#
INCLUDERS = $(patsubst _%.mine,.%,$(wildcard *.mine))
install:: install_includes
install_includes:: | $(HOME)/$(REL) $(ORIGINALS)
	for f in $(INCLUDERS); do						\
	    if [ ! -e  $(ORIGINALS)/.$$f ] && [ -e $(DST)/.$$f ]; then 		\
		cp $(DST)/.$$f $(ORIGINALS);					\
	    else								\
		touch $(ORIGINALS)/.$$f;					\
	    fi;									\
	    if grep -s .$$f.mine $(DST)/.$$f; then :;				\
	    else echo include $(DST)/.$$f.mine >> $(DST)/.$$f;			\
	    fi									\
	done

# Remove broken symlinks in $(HOME)
cleanup::
	@for f in $(HOME)/.*; do			\
	    if [ -L $$f -a ! -e $$f ]; then		\
	    	echo removing broken symlink $$f;	\
	    	rm $$f;					\
	    fi						\
	done

### If we're using the MakeStuff package, chain in its Makefile
CHAIN = $(wildcard ../../MakeStuff/Makefile)
include $(CHAIN)

report-vars::
	@echo CFG= $(CFG)
	@echo SRCFILES=$(SRCFILES)
	@echo SRCDIRS=$(SRCDIRS)
	@echo NONLINKS=$(NONLINKS)
	@echo INCLUDERS=$(INCLUDERS)
	@echo CHAIN=$(CHAIN)
