[![Hackage version](https://img.shields.io/hackage/v/BNFC.svg?label=Hackage)](http://hackage.haskell.org/package/BNFC) <!-- [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/BNFC/badge)](https://matrix.hackage.haskell.org/package/BNFC) -->
[![BNFC on Stackage Nightly](https://stackage.org/package/BNFC/badge/nightly)](https://stackage.org/nightly/package/BNFC)
[![Stackage LTS version](https://www.stackage.org/package/BNFC/badge/lts?label=Stackage)](https://www.stackage.org/package/BNFC)
[![Build status](https://github.com/BNFC/bnfc/workflows/Haskell-CI/badge.svg)](https://github.com/BNFC/bnfc/actions)
[![Documentation status](https://readthedocs.org/projects/bnfc/badge/?version=latest)](http://bnfc.readthedocs.io/en/latest/?badge=latest)

The BNF Converter
=================

What is the BNF Converter?
--------------------------

The BNF Converter (bnfc) is a compiler construction tool generating a compiler
front-end from a Labelled BNF (LBNF) grammar. It is currently able to generate Haskell,
Agda, C, C++, Java, and OCaml, as well as XML representations.

Given a LBNF grammar the tool produces:
- an abstract syntax implementation
- a case skeleton for the abstract syntax in the same language
- an Alex, Ocamllex, JLex, or Flex lexer generator file
- a Happy, Ocamlyacc, Menhir, ANTLR, CUP, or Bison parser generator file
- a pretty-printer as a Haskell/Agda/C/C++/Java/Ocaml module
- a Latex file containing a readable specification of the language

*More information*: http://bnfc.digitalgrammars.com/

Installation
------------

Some binaries are available at https://github.com/BNFC/bnfc/releases.
Installation from the Haskell sources is possible via `stack` or `cabal`.

### Installation via stack (recommended)

You need a running installation of
[stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).
To install and run the latest version of [bnfc from
stackage](https://www.stackage.org/package/BNFC),
enter at the command line:

```
  stack install BNFC
  bnfc --help
```

### Installation via cabal

You need a running installation of a recent version of
[GHC](https://www.haskell.org/ghc/) and
[Cabal](https://www.haskell.org/cabal/), most easily available via the
[Haskell Platform](https://www.haskell.org/platform/).  To install and
[bnfc from hackage](https://hackage.haskell.org/package/BNFC),
enter at the command line:
```
  cabal install BNFC
  bnfc --help
```

### Installing the development version

To install the [development version of
bnfc](https://github.com/BNFC/bnfc) with the latest bugfixes (and
regressions ;-)):
```
  git clone https://github.com/BNFC/bnfc.git
  cd bnfc/source
```
and then either
```
  cabal install
```
or
```
  stack install --stack-yaml stack-8.10.3.yaml
```
(replace `8.10.3` with your GHC version, and if you want to build with
your installed GHC then add flag `--system-ghc`).

Mini tutorial
-------------

- Build a first parser in 5 min (Haskell backend):

  1. In a fresh directory, prepare a grammar file `Sum.cf` with the following content:
     ```
     EInt.  Exp ::= Integer;
     EPlus. Exp ::= Exp "+" Integer;
     ```
  2. Build a parser (in Haskell) with bnfc:
     ```
     bnfc -d -m Sum.cf  &&  make
     ```
     The `make` step needs the Haskell compiler [GHC](https://www.haskell.org/ghc/), the lexer
     generator [alex](https://www.haskell.org/alex/) and the parser generator [happy](https://www.haskell.org/happy/) (all included in the GHC installation).

  3. Inspect the generated files in directory `Sum`.
  4. Test the parser.
     ```
     echo "1 + 2 + 3" | Sum/Test
     ```

- Try the C-family backends. (The prerequisites, GNU C(++) compiler
  (`gcc` / `g++`), lexer generator `flex` and parser generator `bison`,
  are usually present):
  ```
  bnfc --c   -m -o sum-c   Sum.cf  &&  make -C sum-c    &&  echo "1 + 2 + 3" | sum-c/TestSum
  bnfc --cpp -m -o sum-cpp Sum.cf  &&  make -C sum-cpp  &&  echo "1 + 2 + 3" | sum-cpp/TestSum
  ```

- Try the other backends:

  | Option  | Backend  |
  | --- | --- |
  | `--java` | Requires Java, [JLex](https://www.cs.princeton.edu/~appel/modern/java/JLex/) or [JFlex](https://jflex.de/), and [CUP](http://www2.cs.tum.edu/projects/cup/).|
  | `--java-antlr` | Requires [ANTLR](https://www.antlr.org/).|
  | `--ocaml` | Requires [OCaml](https://ocaml.org/), `ocamllex` and `ocamlyacc`.|
  | `--ocaml-menhir` | Uses [menhir](http://gallium.inria.fr/~fpottier/menhir/) instead of `ocamlyacc`.|
  | `--agda` | Produces [Agda](https://agda-lang.org) bindings to the parser generated for Haskell.|
  | `--pygments` | Produces a lexer definition for the Python highlighting suite [Pygments](https://pygments.org/).|

Documentation
-------------

https://bnfc.readthedocs.org/en/latest/

Support
-------

You can discuss with us issues around bnfc on our mailing list bnfc-dev@googlegroups.com.

For current limitations of bnfc, or to report a new bug, please consult our [issue tracker](https://github.com/BNFC/bnfc/issues).

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

License
-------

The project is licensed under the [BSD 3-clause license](https://raw.githubusercontent.com/BNFC/bnfc/master/source/LICENSE).

BNFC versions until 2.8.4 released under the
[GNU General Public License](https://www.gnu.org/licenses/gpl-2.0.html).

Example uses of the BNF Converter
---------------------------------

In research:
- NASA's [OGMA](https://github.com/nasa/ogma) tool uses LBNF for its grammars, e.g. for a [subset of C 99](https://github.com/nasa/ogma/blob/49e78e4d6fa7558d09d36a284648087df48714e4/ogma-language-c/grammar/C.cf).

In teaching:
- Course [Programming Language Technology](http://www.cse.chalmers.se/edu/course/DAT151/) at Chalmers / Gothenburg University.
