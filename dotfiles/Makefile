### Makefile for dotfiles
#

DST = $(HOME)
REL = Config/dotfiles
CFG = $(shell dirname `/bin/pwd`)

DOTFILES = $(wildcard _*)

.PHONY: install

### Install dotfiles in $(HOME).
#
install::	| $(HOME)/$(REL)
	for f in $(DOTFILES); do (cd $(DST) 				\
				 ; ln -nsf $(REL)/$$f `echo $$f|tr _ .`	\
				 ) done

# We may have installed or changed .fonts, so update the cache
install::
	fc-cache ~/.fonts/

$(HOME)/$(REL):
	echo "linking Config->$(CFG)"
	(cd $(HOME); ln -s $(CFG) .)

# Remove broken symlinks in $(HOME)
cleanup::
	@for f in $(HOME)/.*; do			\
	    if [ -L $$f -a ! -e $$f ]; then		\
	    	echo removing broken symlink $$f;	\
	    	rm ~/.gitignore;			\
	    fi						\
	done

### If we're using the Tools package, chain in its Makefile
#	See ../Makefile for more information about Tools.
#
CHAIN = $(wildcard ../../Tools/Makefile)
ifneq ($(CHAIN),)
    include ../../Tools/Makefile
endif

report-vars::
	@echo CFG= $(CFG)
	@echo DOTFILES=$(DOTFILES)
	@echo CHAIN=$(CHAIN)
