### depends.make for Config/emacs
#

.PHONY: install

### Install target
#
install:
	git clone git://github.com/hober/ljupdate.git
	(cd ljupdate; make fetch compile)

