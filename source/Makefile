BNFC_VERSION=$(shell sed -ne "s/^[Vv]ersion: *\([0-9.]*\).*/\1/p" BNFC.cabal)

# Cabal options (to be overwritten from the command line)
CABAL_OPTS=

# Name for binary distribution (e.g. bnfc-2.4.5-linux32)
BDIST_TAG=bnfc-${BNFC_VERSION}-$(shell uname -s)-$(shell uname -m)

.PHONY: build install doc test

all: build

build:
	cabal configure && cabal build

install:
	cabal install

test:
	cabal configure --enable-tests
	cabal test

doc:
	cabal haddock --executables --internal

# Binary package (tgz, for linux)
bdist: dist/${BDIST_TAG}.tar.gz
.PHONY: bdist

dist/%.tar.gz:
	cabal clean
	cabal install ${CABAL_OPTS} --only-dependencies
	cabal configure ${CABAL_OPTS} --prefix=/
	cabal build ${CABAL_OPTS}
	cabal copy --destdir=dist/install
	mkdir dist/$*
	cp dist/install/bin/bnfc dist/$*
	cp LICENSE dist/$*
	tar -cvz -C dist $* > $@

# Print the bnfc version from the cabal file
show-version:
	@echo ${BNFC_VERSION}
