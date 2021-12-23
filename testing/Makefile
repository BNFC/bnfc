.PHONY: build license test usage

test: build
	cabal run

usage: build
	cabal run --help

license: build
	cabal run --license

build:
	make -C src
	cabal build
#--allow-newer

# EOF
