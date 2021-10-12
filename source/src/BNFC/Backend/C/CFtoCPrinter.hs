{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-
    BNF Converter: C Pretty Printer printer
    Copyright (C) 2004  Author:  Michael Pellauer

    Description   : This module generates the C Pretty Printer.
                    It also generates the "show" method for
                    printing an abstract syntax tree.

                    The generated files use the Visitor design pattern.

    Author        : Michael Pellauer
    Created       : 10 August, 2003
    Modified      : 3 September, 2003
                    * Added resizable buffers
-}

module BNFC.Backend.C.CFtoCPrinter (cf2CPrinter) where

import Prelude hiding ((<>))

import Data.Bifunctor ( second )
import Data.Char      ( toLower )
import Data.Either    ( lefts )
import Data.Foldable  ( toList )
import Data.List      ( nub )

import BNFC.CF
import BNFC.PrettyPrint
import BNFC.Utils     ( (+++), uniqOn, unless, unlessNull )

import BNFC.Backend.Common
import BNFC.Backend.Common.NamedVariables
import BNFC.Backend.Common.StrUtils (renderCharOrString)

-- | Produces (.h file, .c file).

cf2CPrinter :: CF -> (String, String)
cf2CPrinter cf = (mkHFile cf groups, mkCFile cf groups)
 where
    groups = fixCoercions $ filterOutDefs $ ruleGroupsInternals cf
    filterOutDefs = map $ second $ filter $ not . isDefinedRule . funRule

{- **** Header (.h) File Methods **** -}

-- | Make the Header File.

mkHFile :: CF -> [(Cat,[Rule])] -> String
mkHFile cf groups = unlines
 [
  header,
  concatMap prPrints eps,
  concatMap prPrintDataH groups,
  concatMap prShows eps,
  concatMap prShowDataH groups,
  footer
 ]
 where
  eps = nub . map normCat . toList $ allEntryPoints cf
  prPrints s | normCat s == s = "char *print" ++ s' ++ "(" ++ s' ++ " p);\n"
    where
      s' = identCat s
  prPrints _ = ""
  prShows s | normCat s == s = "char *show" ++ s' ++ "(" ++ s' ++ " p);\n"
    where
      s' = identCat s
  prShows _ = ""
  header = unlines
   [
    "#ifndef PRINTER_HEADER",
    "#define PRINTER_HEADER",
    "",
    "#include \"Absyn.h\"",
    "",
    "/* Certain applications may improve performance by changing the buffer size */",
    "#define BUFFER_INITIAL 2048",
    "/* You may wish to change _L_PAREN or _R_PAREN */",
    "#define _L_PAREN '('",
    "#define _R_PAREN ')'",
    "",
    "/* The following are simple heuristics for rendering terminals */",
    "/* You may wish to change them */",
    "void renderCC(Char c);",
    "void renderCS(String s);",
    "void indent(void);",
    "void backup(void);",
    "void onEmptyLine(void);",
    "void removeTrailingSpaces(void);",
    "void removeTrailingWhitespace(void);",
    ""
   ]
  footer = unlines $
   ["void pp" ++ t ++ "(String s, int i);" | t <- tokenNames cf]
    ++
   ["void sh" ++ t ++ "(String s);" | t <- tokenNames cf]
    ++
   [
    "void ppInteger(Integer n, int i);",
    "void ppDouble(Double d, int i);",
    "void ppChar(Char c, int i);",
    "void ppString(String s, int i);",
    "void ppIdent(String s, int i);",
    "void shInteger(Integer n);",
    "void shDouble(Double d);",
    "void shChar(Char c);",
    "void shString(String s);",
    "void shIdent(String s);",
    "void bufAppendS(const char *s);",
    "void bufAppendC(const char c);",
    "void bufReset(void);",
    "void resizeBuffer(void);",
    "",
    "#endif"
   ]

-- | Prints all the required method names and their parameters.

prPrintDataH :: (Cat, [Rule]) -> String
prPrintDataH (cat, _) = concat ["void pp", cl, "(", cl, " p, int i);\n"]
  where
   cl = identCat (normCat cat)

-- | Prints all the required method names and their parameters.

prShowDataH :: (Cat, [Rule]) -> String
prShowDataH (cat, _) = concat ["void sh", cl, "(", cl, " p);\n"]
  where
   cl = identCat (normCat cat)

{- **** Implementation (.C) File Methods **** -}

-- | This makes the .C file by a similar method.

mkCFile :: CF -> [(Cat,[Rule])] -> String
mkCFile cf groups = concat
   [
    header,
    prRender,
    concatMap prPrintFun eps,
    concatMap prShowFun eps,
    concatMap prPrintData groups,
    printBasics,
    printTokens,
    concatMap prShowData groups,
    showBasics,
    showTokens,
    footer
   ]
  where
    eps = nub . map normCat . toList $ allEntryPoints cf
    header = unlines
     [
      "/*** Pretty Printer and Abstract Syntax Viewer ***/",
      "",
      "#include <ctype.h>   /* isspace */",
      "#include <stddef.h>  /* size_t */",
      "#include <stdio.h>",
      "#include <string.h>",
      "#include <stdlib.h>",
      "#include \"Printer.h\"",
      "",
      "#define INDENT_WIDTH 2",
      "",
      "int _n_;",
      "char *buf_;",
      "size_t cur_;",
      "size_t buf_size;",
      ""
     ]
    printBasics = unlines
     [
      "void ppInteger(Integer n, int i)",
      "{",
      -- https://stackoverflow.com/questions/10536207/ansi-c-maximum-number-of-characters-printing-a-decimal-int
      -- A buffer of 20 characters is sufficient to print the decimal representation
      -- of a 64bit integer.  Might not be needed here, but does not hurt.
      "  char tmp[20];",
      "  sprintf(tmp, \"%d\", n);",
      "  renderS(tmp);",
      "}",
      "void ppDouble(Double d, int i)",
      "{",
      -- https://stackoverflow.com/questions/1701055/what-is-the-maximum-length-in-chars-needed-to-represent-any-double-value
      -- Recommended buffer size is 24 for doubles (IEEE-754):
      -- (*) 17 digits for the decimal representation of the integral part
      -- (*)  5 digits for the exponent
      "  char tmp[24];",
      "  sprintf(tmp, \"%.15g\", d);",
      "  renderS(tmp);",
      "}",
      "void ppChar(Char c, int i)",
      "{",
      "  char tmp[4];",
      "  sprintf(tmp, \"'%c'\", c);",
      "  renderS(tmp);",
      "}",
      "void ppString(String s, int i)",
      "{",
      "  bufAppendC('\\\"');",
      "  bufAppendS(s);",
      "  bufAppendC('\\\"');",
      "  bufAppendC(' ');",
      "}",
      "void ppIdent(String s, int i)",
      "{",
      "  renderS(s);",
      "}",
      ""
     ]
    printTokens = unlines
     [unlines [
      "void pp" ++ t ++ "(String s, int i)",
      "{",
      "  renderS(s);",
      "}",
      ""
      ] | t <- tokenNames cf
     ]
    showBasics = unlines
     [
      "void shInteger(Integer i)",
      "{",
      "  char tmp[20];",
      "  sprintf(tmp, \"%d\", i);",
      "  bufAppendS(tmp);",
      "}",
      "void shDouble(Double d)",
      "{",
      "  char tmp[24];",
      "  sprintf(tmp, \"%.15g\", d);",
      "  bufAppendS(tmp);",
      "}",
      "void shChar(Char c)",
      "{",
      "  bufAppendC('\\'');",
      "  bufAppendC(c);",
      "  bufAppendC('\\'');",
      "}",
      "void shString(String s)",
      "{",
      "  bufAppendC('\\\"');",
      "  bufAppendS(s);",
      "  bufAppendC('\\\"');",
      "}",
      "void shIdent(String s)",
      "{",
      "  bufAppendC('\\\"');",
      "  bufAppendS(s);",
      "  bufAppendC('\\\"');",
      "}",
      ""
     ]
    showTokens = unlines
     [unlines [
      "void sh" ++ t ++ "(String s)",
      "{",
      "  bufAppendC('\\\"');",
      "  bufAppendS(s);",
      "  bufAppendC('\\\"');",
      "}",
      ""
      ] | t <- tokenNames cf
     ]
    footer = unlines
     [
      "void bufAppendS(const char *s)",
      "{",
      "  size_t len = strlen(s);",
      "  size_t n;",
      "  while (cur_ + len >= buf_size)",
      "  {",
      "    buf_size *= 2; /* Double the buffer size */",
      "    resizeBuffer();",
      "  }",
      "  for(n = 0; n < len; n++)",
      "  {",
      "    buf_[cur_ + n] = s[n];",
      "  }",
      "  cur_ += len;",
      "  buf_[cur_] = 0;",
      "}",
      "void bufAppendC(const char c)",
      "{",
      "  if (cur_ + 1 >= buf_size)",
      "  {",
      "    buf_size *= 2; /* Double the buffer size */",
      "    resizeBuffer();",
      "  }",
      "  buf_[cur_] = c;",
      "  cur_++;",
      "  buf_[cur_] = 0;",
      "}",
      "void bufReset(void)",
      "{",
      "  cur_ = 0;",
      "  buf_size = BUFFER_INITIAL;",
      "  resizeBuffer();",
      "  memset(buf_, 0, buf_size);",
      "}",
      "void resizeBuffer(void)",
      "{",
      "  char *temp = (char *) malloc(buf_size);",
      "  if (!temp)",
      "  {",
      "    fprintf(stderr, \"Error: Out of memory while attempting to grow buffer!\\n\");",
      "    exit(1);",
      "  }",
      "  if (buf_)",
      "  {",
      "    strncpy(temp, buf_, buf_size); /* peteg: strlcpy is safer, but not POSIX/ISO C. */",
      "    free(buf_);",
      "  }",
      "  buf_ = temp;",
      "}",
      "char *buf_;",
      "size_t cur_, buf_size;",
      ""
     ]


{- **** Pretty Printer Methods **** -}

-- | An entry point to the printer.

prPrintFun :: Cat -> String
prPrintFun ep | normCat ep == ep = unlines
  [
   "char *print" ++ ep' ++ "(" ++ ep' ++ " p)",
   "{",
   "  _n_ = 0;",
   "  bufReset();",
   "  pp" ++ ep' ++ "(p, 0);",
   "  return buf_;",
   "}"
  ]
 where
  ep' = identCat ep
prPrintFun _ = ""

-- Generates methods for the Pretty Printer

prPrintData :: (Cat, [Rule]) -> String
prPrintData (cat, rules)
  | isList cat = unlines $ concat
    [ [ "void pp" ++ cl ++ "("++ cl +++ vname ++ ", int i)"
      , "{"
      , "  if (" ++ vname +++ "== 0)"
      , "  { /* nil */"
      ]
    , unlessNull (swRules isNilFun) $ \ docs ->
      [ render $ nest 4 $ vcat docs ]
    , [ "  }" ]
    , unlessNull (swRules isOneFun) $ \ docs ->
      [ "  else if (" ++ pre ++ vname ++ "_ == 0)"
      , "  { /* last */"
      , render $ nest 4 $ vcat docs
      , "  }"
      ]
    , unlessNull (swRules isConsFun) $ \ docs ->
      [ "  else"
      , "  { /* cons */"
      , render $ nest 4 $ vcat docs
      , "  }"
      ]
    , [ "}"
      , ""
      ]
    ]
  | otherwise = unlines $ concat
    [ [ "void pp" ++ cl ++ "(" ++ cl ++ " p, int _i_)"
      , "{"
      , "  switch(p->kind)"
      , "  {"
      ]
    , concatMap prPrintRule rules
    , [ "  default:"
      , "    fprintf(stderr, \"Error: bad kind field when printing " ++ catToStr cat ++ "!\\n\");"
      , "    exit(1);"
      , "  }"
      , "}"
      , ""
      ]
    ]
 where
   cl          = identCat (normCat cat)
   vname       = map toLower cl
   pre         = vname ++ "->"
   prules      = sortRulesByPrecedence rules
   swRules f   = switchByPrecedence "i" $
                   map (second $ sep . map text . prPrintRule_ pre) $
                     uniqOn fst $ filter f prules
                     -- Discard duplicates, can only handle one rule per precedence.

-- | Helper function that call the right c function (renderC or renderS) to
-- render a literal string.
--
-- >>> renderX ","
-- renderC(',')
--
-- >>> renderX "---"
-- renderS("---")

renderX :: String -> Doc
renderX sep' = "render" <> char sc <> parens (text sep)
  where (sc, sep) = renderCharOrString sep'


-- | Pretty Printer methods for a rule.

prPrintRule :: Rule -> [String]
prPrintRule r@(Rule fun _ _ _) = unless (isCoercion fun) $ concat
  [ [ "  case is_" ++ fnm ++ ":"
    , "    if (_i_ > " ++ show p ++ ") renderC(_L_PAREN);"
    ]
  , map ("    " ++) $ prPrintRule_ pre r
  , [ "    if (_i_ > " ++ show p ++ ") renderC(_R_PAREN);"
    , "    break;"
    , ""
    ]
  ]
  where
    p   = precRule r
    fnm = funName fun
    pre = concat [ "p->u.", map toLower fnm, "_." ]

-- | Only render the rhs (items) of a rule.

prPrintRule_ :: IsFun a => String -> Rul a -> [String]
prPrintRule_ pre (Rule _ _ items _) = map (prPrintItem pre) $ numVars items

-- | This goes on to recurse to the instance variables.

prPrintItem :: String -> Either (Cat, Doc) String -> String
prPrintItem pre = \case
  Right t -> render (renderX t) ++ ";"
  Left (cat, nt) -> concat
    [ "pp"
    , maybe (identCat $ normCat cat) basicFunName $ maybeTokenCat cat
    , "(", pre, render nt, ", ", show (precCat cat), ");"
    ]

{- **** Abstract Syntax Tree Printer **** -}

-- | An entry point to the printer.

prShowFun :: Cat -> String
prShowFun ep | normCat ep == ep = unlines
  [
   "char *show" ++ ep' ++ "(" ++ ep' ++ " p)",
   "{",
   "  _n_ = 0;",
   "  bufReset();",
   "  sh" ++ ep' ++ "(p);",
   "  return buf_;",
   "}"
  ]
 where
  ep' = identCat ep
prShowFun _ = ""

-- | This prints the functions for Abstract Syntax tree printing.

prShowData :: (Cat, [Rule]) -> String
prShowData (cat, rules) = unlines $
 if isList cat
 then
 [
  "void sh" ++ cl ++ "("++ cl +++ vname ++ ")",
  "{",
  "  bufAppendC('[');",
  "  while(" ++ vname +++ "!= 0)",
  "  {",
  "    if (" ++ vname ++ "->" ++ vname ++ "_)",
  "    {",
  visitMember,
  "      bufAppendS(\", \");",
  "      " ++ vname +++ "=" +++ vname ++ "->" ++ vname ++ "_;",
  "    }",
  "    else",
  "    {",
  visitMember,
  "      " ++ vname ++ " = 0;",
  "    }",
  "  }",
  "  bufAppendC(']');",
  "}",
  ""
 ] -- Not a list:
 else
 [
   "void sh" ++ cl ++ "(" ++ cl ++ " p)",
   "{",
   "  switch(p->kind)",
   "  {",
   concatMap prShowRule rules,
   "  default:",
   "    fprintf(stderr, \"Error: bad kind field when showing " ++ catToStr cat ++ "!\\n\");",
   "    exit(1);",
   "  }",
   "}\n"
 ]
 where
   cl = identCat (normCat cat)
   ecl = identCat (normCatOfList cat)
   vname = map toLower cl
   member = map toLower ecl
   visitMember = "      sh" ++ ecl ++ "(" ++ vname ++ "->" ++ member ++ "_);"

-- | Pretty Printer methods for a rule.

prShowRule :: Rule -> String
prShowRule (Rule fun _ cats _) | not (isCoercion fun) = unlines
  [
   "  case is_" ++ f ++ ":",
   "  " ++ lparen,
   "    bufAppendS(\"" ++ f ++ "\");\n",
   "  " ++ optspace,
   cats',
   "  " ++ rparen,
   "    break;"
  ]
   where
    f = funName fun
    (optspace, lparen, rparen) = if allTerms cats
      then ("","","")
      else ("  bufAppendC(' ');\n", "  bufAppendC('(');\n","  bufAppendC(')');\n")
    cats' = if allTerms cats
        then ""
        else concat (insertSpaces (map (prShowCat f) (lefts $ numVars cats)))
    insertSpaces [] = []
    insertSpaces (x:[]) = [x]
    insertSpaces (x:xs) = if x == ""
      then insertSpaces xs
      else x : "  bufAppendC(' ');\n" : insertSpaces xs
    allTerms [] = True
    allTerms (Left _:_) = False
    allTerms (_:zs) = allTerms zs
prShowRule _ = ""

prShowCat :: Fun -> (Cat, Doc) -> String
prShowCat fnm (cat, nt) = concat
  [ "    sh"
  , maybe (identCat $ normCat cat) basicFunName $ maybeTokenCat cat
  , "(p->u."
  , map toLower fnm
  , "_."
  , render nt
  , ");\n"
  ]

{- **** Helper Functions Section **** -}

-- | The visit-function name of a basic type.

basicFunName :: TokenCat -> String
basicFunName k
  | k `elem` baseTokenCatNames = k
  | otherwise                  = "Ident"

-- | An extremely simple @renderC@ for terminals.

prRender :: String
prRender = unlines $ concat
  [ [
      "/* You may wish to change the renderC functions */",
      "void renderC(Char c)",
      "{",
      "  if (c == '{')",
      "  {",
      "     onEmptyLine();",
      "     bufAppendC(c);",
      "     _n_ = _n_ + INDENT_WIDTH;",
      "     bufAppendC('\\n');",
      "     indent();",
      "  }",
      "  else if (c == '(' || c == '[')",
      "     bufAppendC(c);",
      "  else if (c == ')' || c == ']')",
      "  {",
      "     removeTrailingWhitespace();",
      "     bufAppendC(c);",
      "     bufAppendC(' ');",
      "  }",
      "  else if (c == '}')",
      "  {",
      "     _n_ = _n_ - INDENT_WIDTH;",
      "     onEmptyLine();",
      "     bufAppendC(c);",
      "     bufAppendC('\\n\');",
      "     indent();",
      "  }",
      "  else if (c == ',')",
      "  {",
      "     removeTrailingWhitespace();",
      "     bufAppendC(c);",
      "     bufAppendC(' ');",
      "  }",
      "  else if (c == ';')",
      "  {",
      "     removeTrailingWhitespace();",
      "     bufAppendC(c);",
      "     bufAppendC('\\n');",
      "     indent();",
      "  }",
      "  else if (c == ' ') bufAppendC(c);",
      "  else if (c == 0) return;",
      "  else",
      "  {",
      "     bufAppendC(c);",
      "     bufAppendC(' ');",
      "  }",
      "}",
      "",
      "int allIsSpace(String s)",
      "{",
      "  char c;",
      "  while ((c = *s++))",
      "    if (! isspace(c)) return 0;",
      "  return 1;",
      "}",
      "",
      "void renderS(String s)",
      "{",
      "  if (*s) /* s[0] != '\\0', string s not empty */",
      "  {",
      "    if (allIsSpace(s)) {",
      "      backup();",
      "      bufAppendS(s);",
      "    } else {",
      "      bufAppendS(s);",
      "      bufAppendC(' ');",
      "    }",
      "  }",
      "}",
      "",
      "void indent(void)",
      "{",
      "  int n = _n_;",
      "  while (--n >= 0)",
      "    bufAppendC(' ');",
      "}",
      "",
      "void backup(void)",
      "{",
      "  if (cur_ && buf_[cur_ - 1] == ' ')",
      "    buf_[--cur_] = 0;",
      "}",
      ""
    ]
  , [ "void removeTrailingSpaces()"
    , "{"
    , "  while (cur_ && buf_[cur_ - 1] == ' ') --cur_;"
    , "  buf_[cur_] = 0;"
    , "}"
    , ""
    , "void removeTrailingWhitespace()"
    , "{"
    , "  while (cur_ && (buf_[cur_ - 1] == ' ' || buf_[cur_ - 1] == '\\n')) --cur_;"
    , "  buf_[cur_] = 0;"
    , "}"
    , ""
    , "void onEmptyLine()"
    , "{"
    , "  removeTrailingSpaces();"
    , "  if (cur_ && buf_[cur_ - 1 ] != '\\n') bufAppendC('\\n');"
    , "  indent();"
    , "}"
    ]
  ]
