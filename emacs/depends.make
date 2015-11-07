### depends.make for Config/emacs
#

### Prevent make push from trying to push ljupdate

GITDIRS :=

.PHONY: install

### Install target
#
install:
	git clone git://github.com/hober/ljupdate.git
	(cd ljupdate; make fetch compile)

