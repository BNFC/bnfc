all: build

build:
	cabal configure --enable-tests && cabal build

test: build
	cabal test

install:
	cabal install

