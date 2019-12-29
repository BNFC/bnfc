bnfc-system-tests
=================

Caveat
------

2018-11-04: GHC 8.4 compilation.

Version at least 0.13.2.4 of the Haskell Test Framework (HTF) is
needed to compile under ghc-8.4.  However, this version does not build
with the latest QuickCheck.  Thus, install HTF separately as follows
before following the general instructions below.
```
  cabal install HTF --constraint=QuickCheck==2.11.3
```
See https://github.com/skogsbaer/HTF/issues/69


Instructions
------------

In this directory (the `testing` directory), run:
```
  cabal install
  bnfc-system-tests
```
It is important to be in this directory, and further:

- In your `CLASSPATH` have `.` and the jars for:
  * JFLex
  * java-cup
  * java-cup-runtime
  * antlr
  * antlr-runtime

- On Mac OS X, Xcode may come with an old version of GNU bison like 2.3.
  `brew install bison` to get a newer one (like 3.2) and make sure it is
  in your PATH.

- Have installed and in the PATH:
  * `jflex`
  * `alex`, `happy`, `hlint`
  * `ocamlc`, `ocamllex`, ``menhir`
  * `agda`
