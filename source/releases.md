# 2.9.0

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

This release adds the __OCaml/Menhir__ backend, a variant of the OCaml backend that uses parser generator `menhir` instead of `ocamlyacc`.  Beyond that, the focus of this release has been increased __robustness and predictability__ of the BNFC tool:

* New option `--check` that only performs sanity checks on the given LBNF grammar file, without calling any backend (#286).
* New sanity checks (#186, #213, #214) and deprecation warnings.
* Error messages of BNFC concerning rule or category names are now equipped with the error location.
* Generated code systematically tries to avoid clashes with language keywords or standard library names (#278, #289).
* Pragma `define` now supported by all maintained backends (Agda, Haskell, Haskell/GADT, C, C++, Java).
* Pragma `position token` now supported by all maintained backends with the exception of C++/NoSTL.
* List separators consisting just of whitespace are now accepted. They are used in the generated printers but treated like "" in the generated parsers (#70).

The Haskell backend has seen the following improvements:

* New option `--text-token` to use `Data.Text` instead of `String` in the generated lexer (#167).
* Deriving `IsString` instances for `Ident`-like token types (#192).
* The `Err` monad is now defined as `Either String`.  Module `ErrM` is only generated for backwards compatibility.  Imports of `ErrM` with an explicit import list may have to modified slightly, see a migration guide at the [Haskell backend documentation](https://bnfc.readthedocs.io/en/v2.8.4/user_guide.html#haskell-backend).
* Block comments delimiters are no longer restricted to 2 characters.

The C-family backends have been improved as follows:

* C, C++, Java: Now multiple block comment forms per grammar are allowed (#202).
* C++/STL: The parser now throws an exception when parsing fails (#288).
* Java/JFlex: Special characters are properly escaped (#299).
* Java/ANTLR: Start rules are generated to work around ANTLR issue #2689 (#272).

Further bug fixes: #163, #169, #196, #212, #235, #256, #264, #269, #274, #276, #277, #279, #282, #283, #290.

Building of BNFC 2.8.4 requires __GHC ≥ 7.10__ and has been tested up to GHC 8.10.2.
BNFC can be build using `cabal` or `stack` (using one of the enclosed `stack-x.y.z.yaml` files).


# 2.8.3

Main new feature:

* Agda backend: generates bindings to output of Haskell backend

Backend-independent features:

* GHC 8.8 compatibility
* Stack installation supported by provided `.yaml` files [#198]
* Unicode support in lexer, excl. C, C++ and Ocaml [#249]
* LBNF: support `\r` (carriage return) and `\f` (form feed) in token declaration [#257]
* LBNF: allow numbered categories (like `Foo2`) independent of coercions [#210]

C backend:

* supports now the `-l` option to give error locations [#238]
* correct function names in generated skeletons [#239]
* handle regular expression difference (subtraction) correctly [#237]

Haskell backend:

* generates now more fine-grained `Makefile` that invokes `bnfc` on changed `.cf` file
* uses qualified import of AST in generated printer [fixes #128,#228]
* printer code no longer produces deprecation warning concerning `OverlappingInstances` [#233]
* `--cnf`: fixed problem with `Any` type in generated code [#216]
* `--cnf`: generated test program now same name (`Test`) as without `--cnf`
* `--glr`: correct module header in .y file [#252]

# 2.8.2

Main new feature:

* Java: An experimental ANTLR4 backend [#155]

Backend-independent features:

* GHC 8.4 compatibility [#227,#236]
* bnfc now puts current timestamp on all created files, even unchanged ones [#219]
* bnfc is now more accepting about border-line terminals [#149]
* Improved error messages [#144] in particular on undefined categories [#151]

C and C++ backends:

* C: Emit correct function prototypes [#185]
* C++: Fix buffer overrun in pretty printer [#242]
* C++: Fix regression introduced in 2.8 in Skeleton.H [#164]
* C++: Replace `%name-prefix` with `%define api.prefix` in bison files [#181]
* C++: Fix a bug that appeared if you had a category named "List"
* C, C++: Add usage output to the test program [#141]
* C, C++: Fix a bug in the parser file when using -p [#172]
* C, C++, OCaml, Java: Accept ' or " as comment delimiters [#146]

Haskell backend:

* Generated code mostly hlint-warning free
* Small fixes [#166,#170,#222]

Other backends:

* Java: Add support for passing line numbers to parser [#217,#224,#226]
* OCaml: Reserved symbols now have a higher precedence than predefined tokens as in Haskell

Further:

* Some updates of the documentation [#211,#223]
* And various small fixes [#139,#159,#195,#201,#215]
