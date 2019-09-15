### Makefile for setup
#

REL = Honu/$(notdir $(shell pwd))

.PHONY: install install-pkgs report-vars


install::
	for f in *setup; do ./$$f; done

install-pkgs::
	for f in *pkgs; do ./$$f; done


### If we're using the MakeStuff package, chain in its Makefile
#	See ../Makefile for more information.
#
CHAIN = $(wildcard ../../MakeStuff/Makefile)
include $(CHAIN)

report-vars::
	@echo REL = $(REL)
	@echo CHAIN=$(CHAIN)
