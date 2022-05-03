.PHONY: build license test usage

SHELL := /bin/bash

test: build exec-test

exec-test:
	cabal v2-install BNFC:exes --overwrite-policy=always
	source ./scripts/env && cabal run bnfc-system-tests #-- "Parameterized tests:C\+\+ \(with namespace\):Examples:cpp"

usage: build
	cabal run bnfc-system-tests -- --help

license: build
	cabal run bnfc-system-tests -- --license

build:
	make -C src
	cabal build
#--allow-newer

# EOF
