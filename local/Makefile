### Makefile for local
#
#   This directory contains files intended to be edited locally.  Sample
#   copies are created by make install, but it is expected that nearly
#   all will be edited to accommodate things like screen sizes, names,
#   email addresses, remote hosts, and the like.
#

MAKE_FILES = local_gitconfig
COPY_FILES = $(addprefix local,$(wildcard _*))
FILES =  $(COPY_FILES) $(MAKE_FILES)

DST := $(HOME)
CFG := $(shell dirname `/bin/pwd`)
REL := $(notdir $(CFG))/local

.PHONY: install report-vars

### Install dotfiles in $(HOME).
#
install::	| $(FILES)
	@echo local configuration files will probably have to be edited.

clean::
	rm -f $(FILES)

### Config files copied from prototypes:
#
#	Prototypes have names starting with _, which keeps the directory
#	listing from getting too cluttered and makes them easy to identify.
#	Config files derived from them	have names starting with "local_".
#	This one rule does it all in most cases; it can be overridden by
#	writing a specific recipe (cf. MAKE_FILES)
#
local_%: ; cp $(subst local,,$@)  $@


### Config files made using makefile recipes.
#	in most cases we do this so that we can do variable expansion here
#	in Makefile, where it's easy.  For that matter, we have all the power
#	of make rules at our command.  In some cases we just do it because
#	templates are easy and we want to avoid clutter.
#
local_gitconfig:
	echo "# edit git user name and email as needed." >$@
	@echo "$$local_gitconfig" >> $@

define local_gitconfig
[user]
	name = Steve Savitzky
	email = steve@savitzky.net
endef
export local_gitconfig


### If we're using the MakeStuff package, chain in its Makefile
#	See ../Makefile for more information.
#
CHAIN = $(wildcard ../../MakeStuff/Makefile)
include $(CHAIN)

report-vars::
	@echo CFG= $(CFG)
	@echo REL= $(REL)
	@echo CHAIN=$(CHAIN)
