#!/bin/sh

#Skip the testsuite
make CABAL_CONFIGURE_OPTS= CABAL_BUILDDIR_SUFFIX=-quick install
