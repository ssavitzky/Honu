### depends.make for dotfiles
#

.PHONY: install

### Install dotfiles in $(HOME).
#   Eventually we should eliminate install.sh in favor of a pure make process.
#
install:
	install.sh
