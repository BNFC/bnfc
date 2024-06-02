# Unreleased

* C: preserve case in constructors (union): e.g. label `EInt` now is union member `eInt_` rather than `eint_`
  [[#479](https://github.com/BNFC/bnfc/issues/479)]

# 2.9.5

Andreas Abel <andreas.abel@gu.se>,  July 2023

* C/C++/Java: escape newline etc. when printing `String` and `Char` literals [[#449](https://github.com/BNFC/bnfc/issues/449)]
* Java/ANTLR: unescape `String` and `Char` literals in parser (needs Java ≥ 15) [[#451](https://github.com/BNFC/bnfc/issues/451)]
* Java/ANTLR: fix case problem with language names like `C` [[#455](https://github.com/BNFC/bnfc/issues/455)]
* Java with line numbers: compatibility with `jflex` ≥ 1.8  [[#453](https://github.com/BNFC/bnfc/issues/453)]
* Haskell/GADT: generated `ComposOp.hs` no longer needs `mtl` library [[#438](https://github.com/BNFC/bnfc/pull/438)]
* Ocaml: fixed a crash in printer with unbalanced `}` [[#439](https://github.com/BNFC/bnfc/issues/439)]
* Ocaml: lex escape sequences in `Char` [[#452](https://github.com/BNFC/bnfc/issues/452)]
* Ocaml: removed superfluous `let rec` in the printers for token categories

Tested GHC versions:
* with `cabal`, GHC 7.10 - 9.6
* with `stack`, GHC 8.2  - 9.6

# 2.9.4.1

Andreas Abel <andreas.abel@gu.se>,  December 2022

* C/C++: allow space characters in token definitions again [[#431](https://github.com/BNFC/bnfc/issues/431)]
  (regression in 2.9.0)
* installation: get rid of `cabal-doctest` dependency [[#429](https://github.com/BNFC/bnfc/issues/429)]

Tested GHC versions:
* with `cabal`, GHC 7.10 - 9.4
* with `stack`, GHC 8.2  - 9.4

# 2.9.4

Andreas Abel <andreas.abel@gu.se>,  February 2022

* LBNF: empty token types are now forbidden [#388]
* Agda: support position information via `--functor` [#405]
* C/C++: use `size_t` and `-Wsign-conversion` [#391]
* C++: repair broken `--line-numbers` [#390], regression in 2.9.2 by [#349]
* Haskell: fix a problem with layout stop words and top-level layout [#399,#413]
* Haskell: generated test parser can parse several files now [#400]
* Java: use _L_`.valueOf()` instead of deprecated `new `_L_`()` for literal classes _L_ [#402]
* Ocaml: non-terminals in generated parser are now type-annotated [#407]
* Ocaml: sanitize bound variables in `define`
* Ocaml/Menhir: update parse error mechanism to Menhir 2021/12/30 [#414]

Contributors:
* Michał Radwański [#390,#391]
* Beatrice Vergani [#399,#400]

# 2.9.3

Andreas Abel <andreas.abel@gu.se>,  September 2021

* BNFC now uniformly signs generated files with its version number [#373]
* C/C++: include `stdio.h` in parser header files [#381]
* C++: fixed parser regression in 2.9.2: missing `#include <algorithm>` [#377]
* Ocaml: lex CR as whitespace [see also #376]
* Ocaml: correct position in parse errors [#380]
* Ocaml/Haskell: make printer for lists categories total [#383]

# 2.9.2

Andreas Abel <andreas.abel@gu.se>,  June 2021

## Major improvements

* Haskell: layout keywords can now be stacked on the same line [#354],
  see https://bnfc.readthedocs.io/en/latest/lbnf.html#stacking-layout-keywords
* C: new methods `free_*` and `clone_*` to deallocate and clone syntax trees [#348]
* C/C++ backends now create reentrant parsers [#349]

## Bug fixes and small improvements

* Haskell-GADT: generated code is warning free [#346]
* Haskell: fixes in layout preprocessor [#343,#344,#345,#352,#353]
* Haskell: print `[Char]` correctly, removed method `prtList` [#359]
* Haskell: added missing import [#368], regression introduced in 2.9.1 by [#331]
* C: fixed a space leak when parsing from a string in memory [#347]
* C: removed errorneous `define`d constructors from `Skeleton.c`
* C++: `define`d constructors now reside in `Absyn` [#287]
* Java: `define`d constructor now reside in `AbsynDef.java` [#287]
* Ocaml: fixed translation of nested `define`d constructors
* C/C++/Java: Pre/post/mixfix lists are now printed correctly [#358]
* all: `define`d constructors involving list expressions work now [#363]
* all: printers render braces on their own line [#366]

## Cosmetical changes

* C/C++: instead of `_SYMB_nnn`, more readable token names in lexer & parser


# 2.9.1

Andreas Abel <andreas.abel@gu.se>,  March 2021

## Main new feature

* Haskell: the `--functor` option now produces position-annotated ASTs [#176,#327]. Thanks @Commelina!

## Bug fixes and small improvements

* Haskell: fix generated `Makefile` and test parser for `--glr` mode [#340]
* Haskell(/GADT): generated modules import `Prelude` explicitly, compatible with `{-# LANGUAGE NoImplicitPrelude #-}`
* Haskell: generated code is warning free [#331]
* Haskell: generated printer more robust wrt. identifier clashes [#337]
* Haskell/C: handle token constructors in `define` expressions [#338]
* Java/ANTLR: removed more superfluous quotation in lexer character sets [#329]
* Ocaml: fix syntax error in generated printer [#330]
* LBNF: more sanity checks [#339]
* Tested with GHC 9.0

# 2.9.0

Andreas Abel <andreas.abel@gu.se>,  December 2020

## Major changes

* New license: BSD 3-clause [#308]
* LBNF: removed `delimiters` pragma [#308]
* Haskell: removed options `--alex1`, `--alex2`, `--sharestrings`, `--profile`, and `--cnf` [#265]
* C#: backend removed [#265]

## Bug fixes

* LBNF: allow list categories in `entrypoints` pragma [#306]
* LBNF: report clashes between token and ordinary categories [#323]
* C: `strdup` is not part of C89 standard, `_POSIX_C_SOURCE` required [#318]
* C/C++: fixed buffer overrun in `String` literal lexer [#316]
* C++: fixed regressions (one of them #310) introduced by #288
* C/C++/OCaml: allow unicode characters in token definitions [#324]
* C/OCaml: sanitize grammar file names [#325]
* Java/ANTLR: removed superfluous quotation in lexer character sets [#319]

# 2.8.4

Andreas Abel <andreas.abel@gu.se>,  October 2020

* GHC versions 7.10 - 8.10 supported, dropped GHC 7.6 and 7.8
* LBNF: whitespace list separators are now accepted; treated like "" [#70]
* `define` pragma now implemented by all maintained backends [#266, #285]
* BNFC performs more sanity checks on the grammar, reports errors with source locations [#186, #213, #214]
* option `--check` to only perform sanity checks [#286]
* Backends now more resilient against keyword and name clashes (e.g. via qualified imports) [#278, #289]
* Haskell: new option `--text-token` to use `Data.Text` instead of `String` in the lexer [#167]
* Haskell: allow block comment delimiters of any length [#169, #202]
* Haskell: define `Err` monad as `Either String` [#273], migration guide at https://bnfc.readthedocs.io/en/v2.8.4/user_guide.html#haskell-backend
* Haskell: `IsString` instances for `Ident`-like token types [#192]
* C/C++/Java: support multiple block comment forms per grammar [#202]
* C++(STL): parser now throws exception on parse error [#288]
* C++: fixed quadratic behavior in C++-generated printer (regression in 2.8.2)
* Java: escape JFlex special characters [#299]
* Java/ANTLR: emit start rules to work around ANTLR issue #2689 [#272]
* Ocaml: new flag `--menhir` to generate parser with menhir instead of ocamlyacc
* Bug fixes: #163, #169, #196, #212, #235, #256, #264, #269, #274, #276, #277, #279, #282, #283, #290
* Fact-checking and revising LBNF documentation at https://bnfc.readthedocs.io/en/v2.8.4/lbnf.html

# 2.8.3

Andreas Abel <andreas.abel@gu.se>,  August 2019

* GHC 8.8 compatibility
* Stack installation supported by provided .yaml files [#198]
* Unicode support in lexer, excl. C, C++ and Ocaml [#249]
* LBNF: support \r (carriage return) and \f (form feed) in token declaration [#257]
* LBNF: allow numbered categories (like Foo2) independent of coercions [#210]
* Agda: new (experimental) backend, providing bindings to AST/parser/printer of Haskell backend
* C: supports now the -l option to give error locations [#238]
* C: correct function names in generated skeletons [#239]
* C, C++: handle regular expression difference (subtraction) correctly [#237]
* Haskell: generates now more fine-grained Makefile that invokes bnfc on changed .cf file
* Haskell: use qualified import of AST in generated printer [#128,#228]
* Haskell: printer code no longer produces deprecation warning concerning OverlappingInstances [#233]
* Haskell/CNF: fixed problem with Any type in generated code [#216]
* Haskell/CNF: generated test program now same name (Test) as w/o --cnf
* Haskell/GLR: correct module header in .y file [#252]

# 2.8.2

Andreas Abel <andreas.abel@gu.se>,  November 2018

* GHC 8.4 compatibility [#227,#236]
* bnfc now puts current timestamp on all created files, even unchanged ones [#219]
* bnfc is now more accepting about border-line terminals [#149]
* Improved error messages [#144] in particular on undefined categories [#151]
* C: Emit correct function prototypes [#185]
* C++: Fix buffer overrun in pretty printer [#242]
* C++: Fix regression introduced in 2.8 in Skeleton.H [#164]
* C++: Replace `%name-prefix` with `%define api.prefix` in bison files [#181]
* C++: Fix a bug that appeared if you had a category named "List"
* C, C++: Add usage output to the test program [#141]
* C, C++: Fix a bug in the parser file when using -p [#172]
* C, C++, OCaml, Java: Accept ' or " as comment delimiters [#146]
* Haskell: Generated code mostly hlint-warning free
* Haskell: Small fixes [#166,#170,#222]
* Java: Add an experimental ANTLR4 backend [#155]
* Java: Add support for passing line numbers to parser [#217,#224,#226]
* OCaml: Reserved symbols now have a higher precedence than predefined
  tokens as in Haskell
* Some updates of the documentation [#211,#223]
* And various small fixes [#139,#159,#195,#201,#215]

# 2.8.1

Grégoire Détrez <gregoire.detrez@gu.se>,  February 2016

* Fix compatibility with GHC 7.10.2 and Alex 3.14
* Fixed #160

# 2.8

Grégoire Détrez <gregoire.detrez@gu.se>,  May 2015

* Builds with ghc 7.10.1
* Add support for JFlex (java)
* Add an option to generate files in an other directory
* Add an experimental option that turns the AST into a parametrized functor (in Haskell only)
* New pygment backend to generate syntax highlighters
* Bug fixes

# 2.7.1

Grégoire Détrez <gregoire.detrez@gu.se>, October 2014

* Generated haskell code is now warning free
* Removed unused terminal in happy
* Correctly escape backslashes in symbols
* Fix problem that was preventing custom tokens to work in OCaml if they conflict with the build-in Ident
* BNFC build is also warning free (ghc 7.4.2)
* Test programs return non-zerro exit code on parse error

# 2.7.0.0

Grégoire Détrez <gregoire.detrez@gu.se>, September 2014

* Add token support for Ocaml
* New option parser
* Adds an optional argument to change Makefile name
* Add a --ghc option to derive Data, Typeable, Generic in Haskell
* New online documentation (https://bnfc.readthedocs.org)
* Derive ``Read`` for newtype decls in Haskell
* New option to get the version number --version
* Remove the F# backend
* Remove the Java4 backend
* New Applicative and Alternative instances to ``Err``
* Remove the coupling between building the parser and the pdf from latex
* Improvement to the CNF Backend
* Bug fixes #92, #21, #34, #33, #90, #30, #60

# 2.5.0

Grégoire Détrez, April 2013

# 2.6.0.3

Grégoire Détrez, January 2013

# 2.4.2.1

Andreas Abel <andreas.abel@ifi.lmu.de>, July 2012

# 2.4.2.0

Thomas Hallgren, September 2010

# 2.4.1.1

Markus Forsberg, September 2010
