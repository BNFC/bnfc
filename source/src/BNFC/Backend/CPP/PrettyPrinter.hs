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

import BNFC.CF
import BNFC.Utils
import BNFC.Backend.Common
import BNFC.Backend.Common.OOAbstract
import BNFC.Backend.Common.NamedVariables
import BNFC.Backend.Common.StrUtils (renderCharOrString)
import BNFC.Backend.CPP.Common      (CppStdMode(..))
import BNFC.Backend.CPP.STL.STLUtils
import BNFC.PrettyPrint
import BNFC.Options()

--Produces (.H file, .C file)
cf2CPPPrinter :: CppStdMode -> Bool -> Maybe String -> CF -> String -> (String, String)
cf2CPPPrinter mode useStl inPackage cf hExt =
    (mkHFile mode useStl inPackage cf groups hExt, mkCFile mode useStl inPackage cf groups hExt)
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
mkHFile :: CppStdMode -> Bool -> Maybe String -> CF -> [(Cat,[Rule])] -> String -> String
mkHFile mode useStl inPackage cf groups hExt = unlines
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
    "#include \"Absyn" ++hExt++ "\"",
    "#include <stdio.h>",
    "#include <stddef.h>",
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
    "  void onEmptyLine(void);",
    "  void removeTrailingSpaces(void);",
    "  void removeTrailingWhitespace(void);",
    " public:",
    "  PrintAbsyn(void);",
    "  ~PrintAbsyn(void);",
    "  char *print(Visitable *v);"
   ]
  hdef = nsDefine inPackage "PRINTER_HEADER"
  content = concatMap (prDataH mode useStl) groups
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
    "  size_t cur_, buf_size;",
    "",
    "  void inline bufAppend(const char *s)",
    "  {",
    "    size_t end = cur_ + strlen(s);",
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
    "    if (buf_) delete[] buf_;",
    "    buf_size = " ++ nsDefine inPackage "BUFFER_INITIAL" ++ ";",
    "    buf_ = new char[buf_size];",
    "    memset(buf_, 0, buf_size);",
    "    cur_ = 0;",
    "  }",
    "",
    "  void inline resizeBuffer(void)",
    "  {",
    "    char *temp = new char[buf_size];",
    "    if (buf_)",
    "    {",
    "      strcpy(temp, buf_);",
    "      delete[] buf_;",
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
prDataH :: CppStdMode -> Bool -> (Cat, [Rule]) -> String
prDataH mode useSTL (cat, rules)
  | isList cat = unlines $ concat
     [ [ concat [ "  void visit", cl, "(", cl, " *p);"          ] ]
    , when useSTL
      [ concat [ "  void iter", cl, "(", itty, " i, ", itty, " j);" ] ]
    ]
  | otherwise  = abstract ++ concatMap prRuleH rules
  where
    beyondAnsi = case mode of
      CppStdBeyondAnsi _ -> True
      CppStdAnsi       _ -> False
    cl       = identCat (normCat cat)
    prRuleH  = if beyondAnsi then prRuleHBeyondAnsi else prRuleHAnsi
    itty     = concat [ cl, "::", "const_iterator" ]
    abstract = case lookupRule (noPosition $ catToStr cat) rules of
      Just _ -> ""
      Nothing -> "  void visit" ++ cl ++ "(" ++ cl ++ " *p); /* abstract class */\n"

--Prints all the methods to visit a rule.
prRuleHAnsi :: IsFun f => Rul f -> String
prRuleHAnsi (Rule fun _ _ _) | isProperLabel fun = concat
  ["  void visit", funName fun, "(", funName fun, " *p);\n"]
prRuleHAnsi _ = ""

prRuleHBeyondAnsi :: IsFun f => Rul f -> String
prRuleHBeyondAnsi (Rule fun _ _ _) | isProperLabel fun = concat
  ["  void visit", funName fun, "(", funName fun, " *p);\n"]
prRuleHBeyondAnsi _ = ""

{- **** Implementation (.C) File Methods **** -}

--This makes the .C file by a similar method.
mkCFile :: CppStdMode -> Bool -> Maybe String -> CF -> [(Cat,[Rule])] -> String -> String
mkCFile mode useStl inPackage cf groups hExt = concat
   [
    header,
    nsStart inPackage ++ "\n",
    prRender useStl,
    printEntries,
    concatMap (prPrintData useStl mode inPackage cf) groups,
    printBasics,
    printTokens,
    showEntries,
    concatMap (prShowData useStl mode cabs) groups,
    showBasics,
    showTokens,
    nsEnd inPackage ++ "\n"
   ]
  where
    cabs = cf2cabs cf
    header = unlines
     [
      "/*** Pretty Printer and Abstract Syntax Viewer ***/",
      "",
      "#include <string>",
      "#include \"Printer" ++hExt++ "\"",
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
      "  if (buf_ && strlen(buf_) > 0)",
      "  {",
      "    delete[] buf_;",
      "  }",
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
      "  if (buf_ && strlen(buf_) > 0)",
      "  {",
      "    delete[] buf_;",
      "  }",
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
prPrintData :: Bool -> CppStdMode -> Maybe String -> CF -> (Cat, [Rule]) -> String

prPrintData True mode _ cf (cat@(ListCat _), rules) =
  render $ genPrintVisitorList (mode, cat, rules, cf)

prPrintData False mode _ cf (cat@(ListCat _), rules) =
  genPrintVisitorListNoStl (mode, cf2cabs cf, cat, rules)

prPrintData _ _ _inPackage cf (TokenCat cat, _rules) |
  isPositionCat cf cat = genPositionToken cat

prPrintData _ mode inPackage cf (cat, rules) =
  abstract ++ concatMap (prPrintRule mode cabs inPackage) rules
  where
    cl = identCat (normCat cat)
    cabs = cf2cabs cf
    abstract = case lookupRule (noPosition $ catToStr cat) rules of
      Just _ -> ""
      Nothing -> "void PrintAbsyn::visit" ++ cl ++ "(" ++ cl +++ "*p) {} //abstract class\n\n"

-- | Generate pretty printer visitor for a list category (STL version).
--
genPrintVisitorList :: (CppStdMode, Cat, [Rule], CF) -> Doc
genPrintVisitorList (mode, cat@(ListCat _), rules, cf) = vcat
  [ "void PrintAbsyn::visit" <> lty <> parens (ltyarg <> "*" <+> varg)
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
      [ "if (i == " <> prevJ <> ")"
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
    cabs = cf2cabs cf
    primitives = [c | (c,_) <- basetypes] ++ tokentypes cabs
    cl        = identCat (normCat cat)
    lty       = text cl                   -- List type
    ltyarg    = text cl                   -- List type arg
    itty      = lty <> "::const_iterator" -- Iterator type
    vname     = text $ map toLower cl
    varg      = text $ (map toLower cl)
    prules    = sortRulesByPrecedence rules
    -- Discard duplicates, can only handle one rule per precedence.
    swRules f = switchByPrecedence "_i_" $ map (second $ sep . prListRuleFn) $ uniqOn fst $ filter f prules
    docs0     = swRules isNilFun
    docs1     = swRules isOneFun
    docs2     = swRules isConsFun

    -- | Only render the rhs (items) of a list rule.
    prListRuleFn :: IsFun a => Rul a -> [Doc]
    prListRuleFn (Rule _ _ items _) = for items $ \case
      Right t       -> "render(" <> text (snd (renderCharOrString t)) <> ");"
      Left c
        | Just{} <- maybeTokenCat c
                    -> "visit" <> dat <> "(" <> visitArg <> ");"
        | isList c  -> "iter" <> dat <> "(" <> nextArg <> ");"
        | otherwise -> "(*i)->accept(this);"
        where
        dat = text $ identCat $ normCat c
        bas = show dat
        isPrimitive = elem bas primitives
        nextArg = case mode of
                    CppStdBeyondAnsi _ -> "std::next(i,1), j"
                    CppStdAnsi       _ -> "i+1, j"
        visitArg = case (mode, isPrimitive) of
                     (CppStdBeyondAnsi _, _) -> "*i->get()"
                     (CppStdAnsi       _, _) -> "*i"

    prevJ = case mode of
      CppStdBeyondAnsi _ -> "std::prev(j, 1)"
      CppStdAnsi       _ -> "j-1"

genPrintVisitorList _ = error "genPrintVisitorList expects a ListCat"

genPositionToken :: String -> String
genPositionToken cat = unlines $
  -- a position token
  [ "void PrintAbsyn::visit" ++ cat ++ "(" ++ cat ++ " *p)"
  , "{"
  , "  visitIdent(p->string_);"
  , "}"
  , ""
  ]


-- This is the only part of the pretty printer that differs significantly
-- between the versions with and without STL.
-- The present version has been adapted from CFtoCPrinter.
genPrintVisitorListNoStl :: (CppStdMode, CAbs, Cat, [Rule]) -> String
genPrintVisitorListNoStl (mode, cabs, cat@(ListCat _), rules) = unlines $ concat
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
    prPrintRuleFn :: IsFun a => String -> Rul a -> [String]
    prPrintRuleFn pre (Rule _ _ items _) = map (prPrintItem mode cabs pre) $ numVars items

    cl          = identCat (normCat cat)
    vname       = map toLower cl
    pre         = vname ++ "->"
    prules      = sortRulesByPrecedence rules
    swRules f   = switchByPrecedence "_i_" $
                  map (second $ sep . map text . prPrintRuleFn pre) $
                  uniqOn fst $ filter f prules
                  -- Discard duplicates, can only handle one rule per precedence.



genPrintVisitorListNoStl _ = error "genPrintVisitorListNoStl expects a ListCat"

--Pretty Printer methods for a rule.
prPrintRule :: CppStdMode -> CAbs -> Maybe String -> Rule -> String
prPrintRule mode cabs inPackage r@(Rule fun _ _ _) | isProperLabel fun = unlines $ concat
  [ [ "void PrintAbsyn::visit" ++ visitFunName ++ "(" ++ vararg +++ fnm ++ ")"
    , "{"
    , "  int oldi = _i_;"
    , parenCode "_L_PAREN"
    , ""
    ]
  , prPrintRuleFn (fnm ++ "->") r
  , [ ""
    , parenCode "_R_PAREN"
    , "  _i_ = oldi;"
    , "}"
    , ""
    ]
  ]
  where
    visitFunName = funName fun
    vararg = funName fun ++ "*"
    p = precRule r
    parenCode x = "  if (oldi > " ++ show p ++ ") render(" ++ nsDefine inPackage x ++ ");"
    fnm = "p" --old names could cause conflicts

    prPrintRuleFn :: IsFun a => String -> Rul a -> [String]
    prPrintRuleFn pre (Rule _ _ items _) = map (prPrintItem mode cabs pre) $ numVars items

prPrintRule _ _ _ _ = "" -- note: this is otherwise pattern, is there any good code style?

--This goes on to recurse to the instance variables.
prPrintItem :: CppStdMode -> CAbs -> String -> Either (Cat, Doc) String -> String
prPrintItem _ _ _    (Right t)     = "  render(" ++ snd (renderCharOrString t) ++ ");"
prPrintItem mode cabs pre  (Left (c, nt))
  | Just t <- maybeTokenCat c = "  visit" ++ t ++ "(" ++ pre ++ visitArg ++ ");"
  | isList c                  = "  " ++ setI (precCat c) ++ "visit" ++ elt ++ "(" ++ pre ++ visitArg ++ ");"
  | otherwise                 = "  " ++ setI (precCat c) ++ pre ++ s ++ "->accept(this);"
  where
    elt = identCat $ normCat c
    s   = render nt
    primitives = [c | (c,_) <- basetypes] ++ tokentypes cabs
    visitArg = case (mode, maybeTokenCat c) of
                 (CppStdBeyondAnsi _, Just t)
                   | not $ elem t primitives -> s ++ ".get()" -- not primitive
                   | otherwise -> s                           -- primitive
                 (CppStdBeyondAnsi _, Nothing)
                   | otherwise -> s ++ ".get()"               -- list is not primitive
                 (CppStdAnsi _, _) -> s                       -- ansi using raw pointer


{- **** Abstract Syntax Tree Printer **** -}

--This prints the functions for Abstract Syntax tree printing.
prShowData :: Bool -> CppStdMode -> CAbs -> (Cat, [Rule]) -> String
prShowData True mode _ (cat@(ListCat c), _) = unlines
 [
  "void ShowAbsyn::visit" ++ cl ++ "("++ cl ++ " *" ++ vname ++ ")",
  "{",
  "  for ("++ cl ++"::const_iterator i = " ++
       vname++"->begin() ; i != " ++vname ++"->end() ; ++i)",
  "  {",
  if isTokenCat c
    then "    visit" ++ baseName cl ++ "(" ++visitArg++ ") ;"
    else "    (*i)->accept(this);",
  case mode of
    CppStdBeyondAnsi _ -> "    if (i != std::prev(" ++ vname ++ "->end(), 1)) bufAppend(\", \");"
    CppStdAnsi _       -> "    if (i != " ++ vname ++ "->end() - 1) bufAppend(\", \");",
  "  }",
  "}",
  ""
 ]
  where
    cl    = identCat (normCat cat)
    vname = map toLower cl
    visitArg = case mode of
      CppStdBeyondAnsi _ -> "*i->get()"
      _                  -> "*i"


prShowData False _ _ (cat@(ListCat c), _) =
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
    cl     = identCat (normCat cat)
    ecl    = identCat (normCatOfList cat)
    vname  = map toLower cl
    member = map toLower ecl ++ "_"
    visitMember
      | Just t <- maybeTokenCat c =
          "      visit" ++ t ++ "(" ++ vname ++ "->" ++ member ++ ");"
      | otherwise =
          "      " ++ vname ++ "->" ++ member ++ "->accept(this);"

prShowData _ mode cabs (cat, rules) =  -- Not a list:
  abstract ++ unlines [prShowRule rule isBeyondAnsi cabs | rule <- rules]
  where
    isBeyondAnsi = case mode of
      CppStdBeyondAnsi _ -> True
      CppStdAnsi       _ -> False
    cl = identCat (normCat cat)
    abstract = case lookupRule (noPosition $ catToStr cat) rules of
      Just _ -> ""
      Nothing -> "void ShowAbsyn::visit" ++ cl ++ "(" ++ cl ++ " *p) {} //abstract class\n\n"

--This prints all the methods for Abstract Syntax tree rules.
prShowRule :: IsFun f => Rul f -> Bool -> CAbs -> String
prShowRule (Rule f _ cats _) isBeyondAnsi cabs | isProperLabel f = concat
  [
    "void ShowAbsyn::visit" ++ fun ++ "(" ++ vararg +++ fnm ++ ")\n",
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
    fnm = "p" --other names could cause conflicts
    vararg = funName fun ++ "*"
    (optspace, lparen, rparen, cats')
      | null [ () | Left _ <- cats ]  -- @all isRight cats@, but Data.Either.isRight requires base >= 4.7
                  = ("", "", "", "")
      | otherwise = ("  bufAppend(' ');\n", "  bufAppend('(');\n","  bufAppend(')');\n"
                    , concat (insertSpaces (map prShowCatFn (numVars cats))))
    insertSpaces [] = []
    insertSpaces (x:[]) = [x]
    insertSpaces (x:xs) = if x == ""
      then insertSpaces xs
      else x : "  bufAppend(' ');\n" : insertSpaces xs
    -- To set cpp information, use partial application of function
    prShowCatFn = prShowCat fnm isBeyondAnsi cabs

prShowRule _ _ _ = ""

-- This recurses to the instance variables of a class.
prShowCat :: String -> Bool -> CAbs -> Either (Cat, Doc) String -> String
prShowCat _ _ _  (Right _) = ""
prShowCat fnm isBeyondAnsi cabs (Left (cat, nt))
  | Just t <- maybeTokenCat cat =
      "  visit" ++ t ++ "(" ++ fnm ++ "->" ++ visitArg ++ ");"
  | catToStr (normCat $ strToCat s) /= s =
      unlines [ accept ]
  | otherwise =
      unlines
        [ "  bufAppend('[');"
        , "  if (" ++ fnm ++ "->" ++ s ++ ")" ++ accept
        , "  bufAppend(']');"
        ]
  where
    s = render nt
    accept = "  " ++ fnm ++ "->" ++ s ++ "->accept(this);"
    primitives = [c | (c,_) <- basetypes] ++ tokentypes cabs
    visitArg = case (isBeyondAnsi, maybeTokenCat cat) of
                 (True, Just t)
                   | not $ elem t primitives -> s ++ ".get()" -- not primitive
                   | otherwise -> s                           -- primitive
                 (True, Nothing)
                   | otherwise -> s ++ ".get()"               -- list is not primitive
                 (False, _) -> s                              -- ansi using raw pointer

{- **** Helper Functions Section **** -}

-- from ListIdent to Ident
baseName :: [a] -> [a]
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
      "     onEmptyLine();",
      "     bufAppend(c);",
      "     _n_ = _n_ + INDENT_WIDTH;",
      "     bufAppend('\\n');",
      "     indent();",
      "  }",
      "  else if (c == '(' || c == '[')",
      "     bufAppend(c);",
      "  else if (c == ')' || c == ']')",
      "  {",
      "     removeTrailingWhitespace();",
      "     bufAppend(c);",
      "     bufAppend(' ');",
      "  }",
      "  else if (c == '}')",
      "  {",
      "     _n_ = _n_ - INDENT_WIDTH;",
      "     onEmptyLine();",
      "     bufAppend(c);",
      "     bufAppend('\\n\');",
      "     indent();",
      "  }",
      "  else if (c == ',')",
      "  {",
      "     removeTrailingWhitespace();",
      "     bufAppend(c);",
      "     bufAppend(' ');",
      "  }",
      "  else if (c == ';')",
      "  {",
      "     removeTrailingWhitespace();",
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
    , "  if (cur_ && buf_[cur_ - 1] == ' ')"
    , "    buf_[--cur_] = 0;"
    , "}"
    , ""
    , "void PrintAbsyn::removeTrailingSpaces()"
    , "{"
    , "  while (cur_ && buf_[cur_ - 1] == ' ') --cur_;"
    , "  buf_[cur_] = 0;"
    , "}"
    , ""
    , "void PrintAbsyn::removeTrailingWhitespace()"
    , "{"
    , "  while (cur_ && (buf_[cur_ - 1] == ' ' || buf_[cur_ - 1] == '\\n')) --cur_;"
    , "  buf_[cur_] = 0;"
    , "}"
    , ""
    , "void PrintAbsyn::onEmptyLine()"
    , "{"
    , "  removeTrailingSpaces();"
    , "  if (cur_ && buf_[cur_ - 1 ] != '\\n') bufAppend('\\n');"
    , "  indent();"
    , "}"
    , ""
    ]
  ]
