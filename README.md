The BNF Converter [![Build Status](https://travis-ci.org/BNFC/bnfc.png?branch=master)](https://travis-ci.org/BNFC/bnfc)
=================

What is the BNF Converter?
--------------------------

The BNF Converter (bnfc) is a compiler construction tool generating a compiler
front-end from a Labelled BNF grammar. It is currently able to generate C, C++,
C#, Haskell, Java, and OCaml, as well as XML representations.

Given a Labelled BNF grammar the tool produces:
- an abstract syntax implementation
- a case skeleton for the abstract syntax in the same language
- an Alex, JLex, or Flex lexer generator file
- a Happy, CUP, or Bison parser generator file
- a pretty-printer as a Haskell/Java/C++/C module
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

To install bnfc from git:

    git clone https://github.com/BNFC/bnfc.git
    cd bnfc/source
    cabal install

To download the latest realease, see the bnfc website: http://bnfc.digitalgrammars.com/


Documentation
-------------

https://bnfc.readthedocs.org/en/latest/

Run tests
---------

Here is how to build and run the different test suites for bnfc.

    cabal sandbox init # Cabal >= 1.18 only
    cabal install --only-dependencies --enable-tests
    cabal configure --enable-tests
    cabal test

Contribute
----------

- Issue Tracker: https://github.com/BNFC/bnfc/issues
- Source Code: https://github.com/BNFC/bnfc

Support
-------

If you are having issues, please let us know.
We have a mailing list located at: bnfc-dev@googlegroups.com

License
-------

The project is licensed under the GNU GENERAL PUBLIC LICENSE Version 2.

