.PHONY: build license test usage

test: build
	bnfc-system-tests

usage: build
	bnfc-system-tests --help

license: build
	bnfc-system-tests --license

build:
	make -C src
	cabal v1-install
#--allow-newer

# EOF
