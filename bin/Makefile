### Makefile for dotconfig
#

.PHONY: install

DST = $(HOME)/bin
REL = Config/$(notdir $(shell pwd))

.PHONY: install report-vars

### Install bin files

install::
	cd $(HOME); ln -sf $(REL) .

### If we're using the Tools package, chain in its Makefile
#	See ../Makefile for more information about Tools.
#
CHAIN = $(wildcard ../../Tools/Makefile)
ifneq ($(CHAIN),)
    include ../../Tools/Makefile
endif

report-vars::
	@echo SRC = $SRC
	@echo DST = $DST
	@echo CHAIN=$(CHAIN)
