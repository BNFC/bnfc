{-# LANGUAGE QuasiQuotes #-}
{-|
  Module      : BNFC.Backend.CPPVar.ReadmeGen
  Description : README generator.

  README generator.
-}

module BNFC.Backend.CPPVar.ReadmeGen
  (
    -- * The entrypoint
    makeReadme

    -- * File naming
  , readmeFilename
  ) where

import Data.String.QQ (s)
import Text.PrettyPrint (Doc)
import BNFC.Backend.CPPVar.CPPUtil

-- | The name of the file.
readmeFilename :: String
readmeFilename = "README.md"

-- | The contents (markdown).
makeReadme :: Doc
makeReadme = unlinesToText [s|
# How to use in a program

## All except printing
Include any of these files:
- `Absyn.hpp` - for the abstract syntax tree classes;
- `Locations.hpp` - for `location` and `position` classes;
- `Parser.hpp` - for the parser interface;
- `PatternMatching.hpp` - for the pipe `|` operator overload and
  the `PatternMatch` class. See the file for details.

### Parser interface

To parse a file, use the function:
```cpp
variant<ParseResultVariant, syntax_error> Parse(FILE*, string* optFilename = nullptr);
```
You may supply a filename, and if your AST supports location tracking, the
pointer to the filename will end up in locations.
Of course, the memory must be deallocated by the user.

To parse an in-memory string, use the overload:
```cpp
variant<ParseResultVariant, syntax_error> Parse(string_view, string* optFilename);
```

These functions may return any of the valid entrypoint classes. To conveniently
check that the parsed object is XYZ, use the function:
```cpp
variant<XYZ, syntax_error> ParseAs<XYZ>(..., string* optFilename);
```

Note that this function does not resolve ambiguities among entrypoints in the
grammar.

`syntax_error` reports the `location` of the error as well as a diagnostic
(`what()`).

For a complete example, see `Test.cpp`.

## Printing the AST

There are 4 classes that visualize the abstract syntax tree:
- `ClassicPrettyPrinter.hpp` - a pretty-printer good enough for most cases;
- `ContextFreePrettyPrinter.hpp` - a pretty-printer that tries to be stateless
  (apart from storing the indentation and precedence levels);
- `HaskellPrinter.hpp` - prints a syntax tree as a Haskell expression;
- `SyntaxPrinter.hpp` - prints a syntax tree as an ASCII tree.

The pretty-printers ideally should be polished by the user for their specific
case. `ContextFreePrettyPrinter` is modified simply by editing method
definitions (`operator()`); `ClassicPrettyPrinter` operates on the token level,
so modifications should be made to the `PutToken` method.

## Building

See `Makefile`. If it is absent, re-run BNFC with the `-m` switch.
|]
