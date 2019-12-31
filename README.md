[![Hackage version](https://img.shields.io/hackage/v/BNFC.svg?label=Hackage)](http://hackage.haskell.org/package/BNFC)
[![Stackage version](https://www.stackage.org/package/BNFC/badge/lts?label=Stackage)](https://www.stackage.org/package/BNFC)
[![Build Status](https://travis-ci.org/BNFC/bnfc.svg?branch=master)](https://travis-ci.org/BNFC/bnfc)
[![Documentation Status](https://readthedocs.org/projects/bnfc/badge/?version=latest)](http://bnfc.readthedocs.io/en/latest/?badge=latest)

The BNF Converter
=================

What is the BNF Converter?
--------------------------

The BNF Converter (bnfc) is a compiler construction tool generating a compiler
front-end from a Labelled BNF grammar. It is currently able to generate C, C++,
C#, Haskell, Java, and OCaml, as well as XML representations.

Given a Labelled BNF grammar the tool produces:
- an abstract syntax implementation
- a case skeleton for the abstract syntax in the same language
- an Alex, Ocamllex, JLex, or Flex lexer generator file
- a Happy, Ocamlyacc, Menhir, ANTLR, CUP, or Bison parser generator file
- a pretty-printer as a Haskell/Ocaml/Java/C++/C module
- a Latex file containing a readable specification of the language

*More information*: http://bnfc.digitalgrammars.com/

Requirements
------------

Everything needed to build bnfc is available in the
[haskell platform](https://www.haskell.org/platform/).

On Debian, you can install the dependencies as follows

    sudo apt-get install haskell-platform

On Fedora:

    yum install haskell-platform

Then proceed with the installation.

Installation
------------

Some binaries are available at https://github.com/BNFC/bnfc/releases .

To install bnfc from hackage.haskell.org:

    cabal install BNFC

To install the development version of bnfc:

    git clone https://github.com/BNFC/bnfc.git
    cd bnfc/source
    cabal install


Documentation
-------------

https://bnfc.readthedocs.org/en/latest/

Run tests
---------

Here is how to build and run the different test suites for bnfc.

    cabal sandbox init # Cabal >= 1.18 only
    cabal install --only-dependencies --enable-tests
    cabal configure --enable-tests
    cabal build
    cabal test

Contribute
----------

- Issue Tracker: https://github.com/BNFC/bnfc/issues
- Source Code: https://github.com/BNFC/bnfc
- Haskell coding style guide: https://github.com/andreasabel/haskell-style-guide/
- Some pull request etiquette:
  * Document, document, document!  (See style guide)
  * Include test cases that cover your feature.
  * Include changelog entry.
  * More etiquette: E.g. https://gist.github.com/mikepea/863f63d6e37281e329f8

Support
-------

If you are having issues, please let us know.
We have a mailing list located at: bnfc-dev@googlegroups.com

License
-------

The project is licensed under the GNU GENERAL PUBLIC LICENSE Version 2.
