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

### Run with Docker

The easiest way to run `bnfc-system-tests` is using Docker, see [Install Docker Engine](https://docs.docker.com/engine/install/).

1. Build the bnfc test image:
   ```
   ../scripts/build-image.sh
   ```
2. Run the test:
   ```
   docker run -it bnfc/bnfc-test:9.12.2
   ```

You may would like to run the test again after making some changes to the source code. This can be done by entering the development environment:
```
../scripts/devenv.sh
make -C source install # do not forget to rebuild and install the new bnfc
make -C testing test
```

### Run on the host

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
