{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-
   **************************************************************
    BNF Converter Module

    Description   : This module generates the C++ Pretty Printer.
                    It also generates the "show" method for
                    printing an abstract syntax tree.

                    The generated files use the Visitor design pattern.

    Author        : Michael Pellauer
    Created       : 10 August, 2003
    Modified      : 3 September, 2003
                    * Added resizable buffers

   **************************************************************
-}

module BNFC.Backend.CPP.PrettyPrinter (cf2CPPPrinter, prRender) where

import Prelude hiding ((<>))

import Data.Bifunctor (second)
import Data.Char  (toLower)
import Data.Maybe (isJust)

import BNFC.CF
import BNFC.Utils
import BNFC.Backend.Common
import BNFC.Backend.Common.NamedVariables
import BNFC.Backend.Common.StrUtils (renderCharOrString)
import BNFC.Backend.CPP.STL.STLUtils
import BNFC.PrettyPrint

--Produces (.H file, .C file)
cf2CPPPrinter :: Bool -> Maybe String -> CF -> (String, String)
cf2CPPPrinter useStl inPackage cf =
    (mkHFile useStl inPackage cf groups, mkCFile useStl inPackage cf groups)
 where
    groups = when useStl (positionRules cf)  -- CPP/NoSTL treats position tokens as just tokens
          ++ fixCoercions (ruleGroupsInternals cf)

positionRules :: CF -> [(Cat,[Rule])]
positionRules cf =
  [ (TokenCat cat, [ Rule (noPosition cat) (noPosition $ TokenCat cat) (map (Left . TokenCat) [catString, catInteger]) Parsable ])
  | cat <- filter (isPositionCat cf) $ map fst (tokenPragmas cf)
  ]

{- **** Header (.H) File Methods **** -}

--An extremely large function to make the Header File
mkHFile :: Bool -> Maybe String -> CF -> [(Cat,[Rule])] -> String
mkHFile useStl inPackage cf groups = unlines
  [ printHeader
  , content
  , classFooter
  , showHeader
  , content
  , classFooter
  , footer
  ]
  where
  printHeader = unlines
   [
    "#ifndef " ++ hdef,
    "#define " ++ hdef,
    "",
    "#include \"Absyn.H\"",
    "#include <stdio.h>",
    "#include <string.h>",
    "#include <stdlib.h>",
    "",
    nsStart inPackage,
    "/* Certain applications may improve performance by changing the buffer size */",
    "#define " ++ nsDefine inPackage "BUFFER_INITIAL" ++ " 2000",
    "/* You may wish to change _L_PAREN or _R_PAREN */",
    "#define " ++ nsDefine inPackage "_L_PAREN" ++ " '('",
    "#define " ++ nsDefine inPackage "_R_PAREN" ++ " ')'",
    "",
    "class PrintAbsyn : public Visitor",
    "{",
    " protected:",
    "  int _n_, _i_;",
    "  /* The following are simple heuristics for rendering terminals */",
    "  /* You may wish to change them */",
    "  void render(Char c);",
    if useStl then "  void render(String s);" else "",
    "  void render(const char *s);",
    "  void indent(void);",
    "  void backup(void);",
    " public:",
    "  PrintAbsyn(void);",
    "  ~PrintAbsyn(void);",
    "  char *print(Visitable *v);"
   ]
  hdef = nsDefine inPackage "PRINTER_HEADER"
  content = concatMap (prDataH useStl) groups
  classFooter = unlines $
   [
    "  void visitInteger(Integer i);",
    "  void visitDouble(Double d);",
    "  void visitChar(Char c);",
    "  void visitString(String s);",
    "  void visitIdent(String s);"
   ] ++ ["  void visit" ++ t ++ "(String s);" | t <- tokenNames cf] ++
   [
    " protected:",
    "  char *buf_;",
    "  int cur_, buf_size;",
    "",
    "  void inline bufAppend(const char *s)",
    "  {",
    "    int end = cur_ + strlen(s);",
    "    if (end >= buf_size) {",
    "      do buf_size *= 2; /* Double the buffer size */",
    "      while (end >= buf_size);",
    "      resizeBuffer();",
    "    }",
    "    strcpy(&buf_[cur_], s);",
    "    cur_ = end;",
    "  }",
    "",
    "  void inline bufAppend(const char c)",
    "  {",
    "    if (cur_ + 1 >= buf_size)",
    "    {",
    "      buf_size *= 2; /* Double the buffer size */",
    "      resizeBuffer();",
    "    }",
    "    buf_[cur_] = c;",
    "    buf_[++cur_] = 0;",
    "  }",
    "",
    if useStl then render (nest 2 bufAppendString) else "",
    "  void inline bufReset(void)",
    "  {",
    "    if (buf_) free(buf_);",
    "    buf_size = " ++ nsDefine inPackage "BUFFER_INITIAL" ++ ";",
    "    buf_ = (char *) malloc(buf_size);",
    "    if (!buf_) {",
    "      fprintf(stderr, \"Error: Out of memory while allocating buffer!\\n\");",
    "      exit(1);",
    "    }",
    "    memset(buf_, 0, buf_size);",
    "    cur_ = 0;",
    "  }",
    "",
    "  void inline resizeBuffer(void)",
    "  {",
    "    char *temp = (char *) malloc(buf_size);",
    "    if (!temp)",
    "    {",
    "      fprintf(stderr, \"Error: Out of memory while attempting to grow buffer!\\n\");",
    "      exit(1);",
    "    }",
    "    if (buf_)",
    "    {",
    "      strcpy(temp, buf_);",
    "      free(buf_);",
    "    }",
    "    buf_ = temp;",
    "  }",
    "};",
    ""
   ]
  bufAppendString :: Doc
  bufAppendString =
      "void inline bufAppend(String str)"
      $$ codeblock 2
          [ "const char *s = str.c_str();"
          , "bufAppend(s);"
          ]
  showHeader = unlines
   [
    "",
    "class ShowAbsyn : public Visitor",
    "{",
    " public:",
    "  ShowAbsyn(void);",
    "  ~ShowAbsyn(void);",
    "  char *show(Visitable *v);"
   ]
  footer = unlines
   [
    nsEnd inPackage,
    "",
    "#endif"
   ]

--Prints all the required method names and their parameters.
prDataH :: Bool -> (Cat, [Rule]) -> String
prDataH useSTL (cat, rules)
 | isList cat = unlines $ concat
     [ [ concat [ "  void visit", cl, "(", cl, " *p);"          ] ]
     , when useSTL
       [ concat [ "  void iter", cl, "(", itty, " i, ", itty, " j);" ] ]
     ]
 | otherwise  = abstract ++ concatMap prRuleH rules
 where
   cl       = identCat (normCat cat)
   itty     = concat [ cl, "::", "const_iterator" ]
   abstract = case lookupRule (noPosition $ show cat) rules of
    Just _ -> ""
    Nothing ->  "  void visit" ++ cl ++ "(" ++ cl ++ " *p); /* abstract class */\n"

--Prints all the methods to visit a rule.
prRuleH :: IsFun f => Rul f -> String
prRuleH (Rule fun _ _ _) | isProperLabel fun = concat
  ["  void visit", funName fun, "(", funName fun, " *p);\n"]
prRuleH _ = ""

{- **** Implementation (.C) File Methods **** -}

--This makes the .C file by a similar method.
mkCFile :: Bool -> Maybe String -> CF -> [(Cat,[Rule])] -> String
mkCFile useStl inPackage cf groups = concat
   [
    header,
    nsStart inPackage ++ "\n",
    prRender useStl,
    printEntries,
    concatMap (prPrintData useStl inPackage cf) groups,
    printBasics,
    printTokens,
    showEntries,
    concatMap (prShowData useStl) groups,
    showBasics,
    showTokens,
    nsEnd inPackage ++ "\n"
   ]
  where
    header = unlines
     [
      "/*** BNFC-Generated Pretty Printer and Abstract Syntax Viewer ***/",
      "",
      "#include <string>",
      "#include \"Printer.H\"",
      "#define INDENT_WIDTH 2",
      ""
     ]
    printEntries = unlines
     [
      "PrintAbsyn::PrintAbsyn(void)",
      "{",
      "  _i_ = 0; _n_ = 0;",
      "  buf_ = 0;",
      "  bufReset();",
      "}",
      "",
      "PrintAbsyn::~PrintAbsyn(void)",
      "{",
      "}",
      "",
      "char *PrintAbsyn::print(Visitable *v)",
      "{",
      "  _i_ = 0; _n_ = 0;",
      "  bufReset();",
      "  v->accept(this);",
      "  return buf_;",
      "}",
      ""
     ]
    showEntries = unlines
     [
      "ShowAbsyn::ShowAbsyn(void)",
      "{",
      "  buf_ = 0;",
      "  bufReset();",
      "}",
      "",
      "ShowAbsyn::~ShowAbsyn(void)",
      "{",
      "}",
      "",
      "char *ShowAbsyn::show(Visitable *v)",
      "{",
      "  bufReset();",
      "  v->accept(this);",
      "  return buf_;",
      "}",
      ""
     ]
    printBasics = unlines
     [
      "void PrintAbsyn::visitInteger(Integer i)",
      "{",
      "  char tmp[20];",
      "  sprintf(tmp, \"%d\", i);",
      "  render(tmp);",
      "}",
      "",
      "void PrintAbsyn::visitDouble(Double d)",
      "{",
      "  char tmp[24];",
      "  sprintf(tmp, \"%.15g\", d);",
      "  render(tmp);",
      "}",
      "",
      "void PrintAbsyn::visitChar(Char c)",
      "{",
      "  char tmp[4];",
      "  sprintf(tmp, \"'%c'\", c);",
      "  render(tmp);",
      "}",
      "",
      "void PrintAbsyn::visitString(String s)",
      "{",
      "  bufAppend('\\\"');",
      "  bufAppend(s);",
      "  bufAppend('\\\"');",
      "  bufAppend(' ');",
      "}",
      "",
      "void PrintAbsyn::visitIdent(String s)",
      "{",
      "  render(s);",
      "}",
      ""
     ]

    printTokens = unlines
     [unlines [
      "void PrintAbsyn::visit" ++ t ++ "(String s)",
      "{",
      "  render(s);",
      "}",
      ""
      ] | t <- tokenNames cf
     ]

    showBasics = unlines
     [
      "void ShowAbsyn::visitInteger(Integer i)",
      "{",
      "  char tmp[20];",
      "  sprintf(tmp, \"%d\", i);",
      "  bufAppend(tmp);",
      "}",
      "void ShowAbsyn::visitDouble(Double d)",
      "{",
      "  char tmp[24];",
      "  sprintf(tmp, \"%.15g\", d);",
      "  bufAppend(tmp);",
      "}",
      "void ShowAbsyn::visitChar(Char c)",
      "{",
      "  bufAppend('\\'');",
      "  bufAppend(c);",
      "  bufAppend('\\'');",
      "}",
      "void ShowAbsyn::visitString(String s)",
      "{",
      "  bufAppend('\\\"');",
      "  bufAppend(s);",
      "  bufAppend('\\\"');",
      "}",
      "void ShowAbsyn::visitIdent(String s)",
      "{",
      "  bufAppend('\\\"');",
      "  bufAppend(s);",
      "  bufAppend('\\\"');",
      "}",
      ""
     ]

    showTokens = unlines
     [unlines [
      "void ShowAbsyn::visit" ++ t ++ "(String s)",
      "{",
      "  bufAppend('\\\"');",
      "  bufAppend(s);",
      "  bufAppend('\\\"');",
      "}",
      ""
      ] | t <- tokenNames cf
     ]


{- **** Pretty Printer Methods **** -}

-- | Generates methods for the Pretty Printer.
prPrintData :: Bool -> Maybe String -> CF -> (Cat, [Rule]) -> String
prPrintData True {- use STL -} _ _ (cat@(ListCat _), rules) =
    render $ genPrintVisitorList (cat, rules)
prPrintData False {- use STL -} _ _ (cat@(ListCat _), rules) =
    genPrintVisitorListNoStl (cat, rules)
-- Not a list :
prPrintData _ _inPackage cf (TokenCat cat, _rules) | isPositionCat cf cat = unlines $
  -- a position token
  [ "void PrintAbsyn::visit" ++ cat ++ "(" ++ cat ++ " *p)"
  , "{"
  , "  visitIdent(p->string_);"
  , "}"
  , ""
  ]
prPrintData _ inPackage _cf (cat, rules) = -- Not a list
    abstract ++ concatMap (prPrintRule inPackage) rules
  where
  cl = identCat (normCat cat)
  abstract = case lookupRule (noPosition $ show cat) rules of
    Just _ -> ""
    Nothing ->  "void PrintAbsyn::visit" ++ cl ++ "(" ++ cl +++ "*p) {} //abstract class\n\n"

-- | Generate pretty printer visitor for a list category (STL version).
--
genPrintVisitorList :: (Cat, [Rule]) -> Doc
genPrintVisitorList (cat@(ListCat c), rules) = vcat
  [ "void PrintAbsyn::visit" <> lty <> parens (lty <+> "*" <> vname)
  , codeblock 2
    [ "iter" <> lty <> parens (vname <> "->begin()" <> comma <+> vname <> "->end()") <> semi ]
  , ""
  , "void PrintAbsyn::iter" <> lty <> parens (itty <+> "i" <> comma <+> itty <+> "j")
  , codeblock 2 $ concat
    [ if null docs0 then
      [ "if (i == j) return;" ]
      else
      [ "if (i == j)"
      , "{ /* nil */"
      , nest 2 $ vcat docs0
      , "}"
      , "else"
      ]
    , unless (null docs1)
      [ "if (i == j-1)"
      , "{ /* last */"
      , nest 2 $ vcat docs1
      , "}"
      , "else"
      ]
    , [ "{ /* cons */"
      ,  nest 2 $ vcat docs2
      , "}"
      ]
    ]
  , ""
  , ""
  ]
  where
  cl        = identCat (normCat cat)
  lty       = text cl                   -- List type
  itty      = lty <> "::const_iterator" -- Iterator type
  vname     = text $ map toLower cl
  prules    = sortRulesByPrecedence rules
  swRules f = switchByPrecedence "_i_" $
                map (second $ sep . prListRule_) $
                  uniqOn fst $ filter f prules
                  -- Discard duplicates, can only handle one rule per precedence.
  docs0     = swRules isNilFun
  docs1     = swRules isOneFun
  docs2     = swRules isConsFun

genPrintVisitorList _ = error "genPrintVisitorList expects a ListCat"

-- | Only render the rhs (items) of a list rule.

prListRule_ :: IsFun a => Rul a -> [Doc]
prListRule_ (Rule _ _ items _) = for items $ \case
  Right t       -> "render(" <> text (snd (renderCharOrString t)) <> ");"
  Left c
    | Just t <- maybeTokenCat c
                -> "visit" <> dat <> "(*i);"
    | isList c  -> "iter" <> dat <> "(i+1, j);"
    | otherwise -> "(*i)->accept(this);"
    where
    dat = text $ identCat $ normCat c

-- This is the only part of the pretty printer that differs significantly
-- between the versions with and without STL.
-- The present version has been adapted from CFtoCPrinter.
genPrintVisitorListNoStl :: (Cat, [Rule]) -> String
genPrintVisitorListNoStl (cat@(ListCat c), rules) = unlines $ concat
  [ [ "void PrintAbsyn::visit" ++ cl ++ "("++ cl ++ " *" ++ vname ++ ")"
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
  where
  cl          = identCat (normCat cat)
  vname       = map toLower cl
  pre         = vname ++ "->"
  prules      = sortRulesByPrecedence rules
  swRules f   = switchByPrecedence "_i_" $
                  map (second $ sep . map text . prPrintRule_ pre) $
                    uniqOn fst $ filter f prules
                    -- Discard duplicates, can only handle one rule per precedence.
genPrintVisitorListNoStl _ = error "genPrintVisitorListNoStl expects a ListCat"

--Pretty Printer methods for a rule.
prPrintRule :: Maybe String -> Rule -> String
prPrintRule inPackage r@(Rule fun _ items _) | isProperLabel fun = unlines $ concat
  [ [ "void PrintAbsyn::visit" ++ funName fun ++ "(" ++ funName fun +++ "*" ++ fnm ++ ")"
    , "{"
    , "  int oldi = _i_;"
    , parenCode "_L_PAREN"
    , ""
    ]
  , prPrintRule_ (fnm ++ "->") r
  , [ ""
    , parenCode "_R_PAREN"
    , "  _i_ = oldi;"
    , "}"
    , ""
    ]
  ]
  where
  p = precRule r
  parenCode x = "  if (oldi > " ++ show p ++ ") render(" ++ nsDefine inPackage x ++ ");"
  fnm = "p" --old names could cause conflicts
prPrintRule _ _ = ""

prPrintRule_ :: IsFun a => String -> Rul a -> [String]
prPrintRule_ pre (Rule _ _ items _) = map (prPrintItem pre) $ numVars items

--This goes on to recurse to the instance variables.
prPrintItem :: String -> Either (Cat, Doc) String -> String
prPrintItem _   (Right t) = "  render(" ++ snd (renderCharOrString t) ++ ");"
prPrintItem pre (Left (c, nt))
  | Just t <- maybeTokenCat c
              = "  visit" ++ t   ++ "(" ++ pre ++ s ++ ");"
  | isList c  = "  " ++ setI (precCat c) ++
                  "visit" ++ elt ++ "(" ++ pre ++ s ++ ");"
  | otherwise = "  " ++ setI (precCat c) ++ pre ++ s ++ "->accept(this);"
  where
  s   = render nt
  elt = identCat $ normCat c

{- **** Abstract Syntax Tree Printer **** -}

--This prints the functions for Abstract Syntax tree printing.
prShowData :: Bool -> (Cat, [Rule]) -> String
prShowData True (cat@(ListCat c), _) = unlines
 [
  "void ShowAbsyn::visit" ++ cl ++ "("++ cl ++ " *" ++ vname ++ ")",
  "{",
  "  for ("++ cl ++"::const_iterator i = " ++
       vname++"->begin() ; i != " ++vname ++"->end() ; ++i)",
  "  {",
  if isTokenCat c
    then "    visit" ++ baseName cl ++ "(*i) ;"
    else "    (*i)->accept(this);",
  "    if (i != " ++ vname ++ "->end() - 1) bufAppend(\", \");",
  "  }",
  "}",
  ""
 ]
  where
    cl = identCat (normCat cat)
    vname = map toLower cl
prShowData False (cat@(ListCat c), _) =
 unlines
 [
  "void ShowAbsyn::visit" ++ cl ++ "("++ cl ++ " *" ++ vname ++ ")",
  "{",
  "  while(" ++ vname ++ "!= 0)",
  "  {",
  "    if (" ++ vname ++ "->" ++ vname ++ "_)",
  "    {",
  visitMember,
  "      bufAppend(\", \");",
  "      " ++ vname +++ "=" +++ vname ++ "->" ++ vname ++ "_;",
  "    }",
  "    else",
  "    {",
  visitMember,
  "      " ++ vname ++ " = 0;",
  "    }",
  "  }",
  "}",
  ""
 ]
  where
    cl = identCat (normCat cat)
    ecl = identCat (normCatOfList cat)
    vname = map toLower cl
    member = map toLower ecl ++ "_"
    visitMember
      | Just t <- maybeTokenCat c =
          "      visit" ++ t ++ "(" ++ vname ++ "->" ++ member ++ ");"
      | otherwise =
          "      " ++ vname ++ "->" ++ member ++ "->accept(this);"
prShowData _ (cat, rules) =  --Not a list:
  abstract ++ concatMap prShowRule rules
  where
    cl = identCat (normCat cat)
    abstract = case lookupRule (noPosition $ show cat) rules of
      Just _ -> ""
      Nothing ->  "void ShowAbsyn::visit" ++ cl ++ "(" ++ cl ++ " *p) {} //abstract class\n\n"

--This prints all the methods for Abstract Syntax tree rules.
prShowRule :: IsFun f => Rul f -> String
prShowRule (Rule f _ cats _) | isProperLabel f = concat
  [
   "void ShowAbsyn::visit" ++ fun ++ "(" ++ fun +++ "*" ++ fnm ++ ")\n",
   "{\n",
   lparen,
   "  bufAppend(\"" ++ fun ++ "\");\n",
   optspace,
   cats',
   rparen,
   "}\n"
  ]
   where
    fun = funName f
    (optspace, lparen, rparen, cats')
      | null [ () | Left _ <- cats ]  -- @all isRight cats@, but Data.Either.isRight requires base >= 4.7
                  = ("", "", "", "")
      | otherwise = ("  bufAppend(' ');\n", "  bufAppend('(');\n","  bufAppend(')');\n"
                    , concat (insertSpaces (map (prShowCat fnm) (numVars cats))))
    insertSpaces [] = []
    insertSpaces (x:[]) = [x]
    insertSpaces (x:xs) = if x == ""
      then insertSpaces xs
      else x : "  bufAppend(' ');\n" : insertSpaces xs
    fnm = "p" --other names could cause conflicts
prShowRule _ = ""

-- This recurses to the instance variables of a class.
prShowCat :: String -> Either (Cat, Doc) String -> String
prShowCat _   (Right _) = ""
prShowCat fnm (Left (cat, nt))
  | Just t <- maybeTokenCat cat =
      unlines
        [ "  visit" ++ t ++ "(" ++ fnm ++ "->" ++ s ++ ");"
        ]
  | catToStr (normCat $ strToCat s) /= s =
      unlines
        [ accept
        ]
  | otherwise =
      unlines
        [ "  bufAppend('[');"
        , "  if (" ++ fnm ++ "->" ++ s ++ ")" ++ accept
        , "  bufAppend(']');"
        ]
  where
  s = render nt
  accept = "  " ++ fnm ++ "->" ++ s ++ "->accept(this);"

{- **** Helper Functions Section **** -}

-- from ListIdent to Ident
baseName = drop 4


--Just sets the coercion level for parentheses in the Pretty Printer.
setI :: Integer -> String
setI n = "_i_ = " ++ show n ++ "; "

--An extremely simple renderer for terminals.
prRender :: Bool -> String
prRender useStl = unlines $ concat
  [ [
      "//You may wish to change render",
      "void PrintAbsyn::render(Char c)",
      "{",
      "  if (c == '{')",
      "  {",
      "     bufAppend('\\n');",
      "     indent();",
      "     bufAppend(c);",
      "     _n_ = _n_ + INDENT_WIDTH;",
      "     bufAppend('\\n');",
      "     indent();",
      "  }",
      "  else if (c == '(' || c == '[')",
      "     bufAppend(c);",
      "  else if (c == ')' || c == ']')",
      "  {",
      "     backup();",
      "     bufAppend(c);",
      "     bufAppend(' ');",
      "  }",
      "  else if (c == '}')",
      "  {",
      "     int t;",
      "     _n_ = _n_ - INDENT_WIDTH;",
      "     for (t=0; t<INDENT_WIDTH; t++) {",
      "       backup();",
      "     }",
      "     bufAppend(c);",
      "     bufAppend('\\n\');",
      "     indent();",
      "  }",
      "  else if (c == ',')",
      "  {",
      "     backup();",
      "     bufAppend(c);",
      "     bufAppend(' ');",
      "  }",
      "  else if (c == ';')",
      "  {",
      "     backup();",
      "     bufAppend(c);",
      "     bufAppend('\\n');",
      "     indent();",
      "  }",
      "  else if (c == ' ') bufAppend(c);",
      "  else if (c == 0) return;",
      "  else",
      "  {",
      "     bufAppend(c);",
      "     bufAppend(' ');",
      "  }",
      "}",
      ""
    ]
  , when useStl
    [ render $ vcat
        [ "void PrintAbsyn::render(String s)"
        , codeblock 2
            [ "render(s.c_str());"
            ]
        , ""
        ]
    ]
  , [ "bool allIsSpace(const char *s)"
    , "{"
    , "  char c;"
    , "  while ((c = *s++))"
    , "    if (! isspace(c)) return false;"
    , "  return true;"
    , "}"
    , ""
    ]
  , [ "void PrintAbsyn::render(const char *s)"
    , "{"
    , "  if (*s) /* C string not empty */"
    , "  {"
    , "    if (allIsSpace(s)) {"
    , "      backup();"
    , "      bufAppend(s);"
    , "    } else {"
    , "      bufAppend(s);"
    , "      bufAppend(' ');"
    , "    }"
    , "  }"
    , "}"
    , ""
    , "void PrintAbsyn::indent()"
    , "{"
    , "  int n = _n_;"
    , "  while (--n >= 0)"
    , "    bufAppend(' ');"
    , "}"
    , ""
    , "void PrintAbsyn::backup()"
    , "{"
    , "  if (buf_[cur_ - 1] == ' ')"
    , "    buf_[--cur_] = 0;"
    , "}"
    , ""
    ]
  ]
