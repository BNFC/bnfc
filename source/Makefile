.PHONY: build install doc

all: build

build:
	cabal configure && cabal build

install:
	cabal install

doc:
	cabal haddock --executables --internal
