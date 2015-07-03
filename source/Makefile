.PHONY: build install doc

all: build

build:
	cabal configure && cabal build

install:
	cabal install

test:
	cabal test

doc:
	cabal haddock --executables --internal
