{-# LANGUAGE NoImplicitPrelude #-}

{-
   **************************************************************
    BNF Converter Module

    Description   : This module generates the C++ Pretty Printer.
                    It also generates the "show" method for
                    printing an abstract syntax tree.

                    The generated files use the Visitor design pattern.

    Author        : Michael Pellauer (pellauer@cs.chalmers.se)

    License       : GPL (GNU General Public License)

    Created       : 10 August, 2003

    Modified      : 3 September, 2003
                    * Added resizable buffers

   **************************************************************
-}

module BNFC.Backend.CPP.PrettyPrinter (cf2CPPPrinter, prRender) where

import Prelude'

import BNFC.CF
import BNFC.Utils ((+++))
import BNFC.Backend.Common
import BNFC.Backend.Common.NamedVariables
import BNFC.Backend.Common.StrUtils (renderCharOrString)
import Data.Char(toLower)
import BNFC.Backend.CPP.STL.STLUtils
import BNFC.PrettyPrint

--Produces (.H file, .C file)
cf2CPPPrinter :: Bool -> Maybe String -> CF -> (String, String)
cf2CPPPrinter useStl inPackage cf =
    (mkHFile useStl inPackage cf groups, mkCFile useStl inPackage cf groups)
 where
    groups = positionRules cf ++ fixCoercions (ruleGroupsInternals cf)

positionRules :: CF -> [(Cat,[Rule])]
positionRules cf =
      [(cat,[Rule (show cat) cat [Left catString, Left catInteger]]) |
        cat <- filter (isPositionCat cf) $ fst (unzip (tokenPragmas cf))]

{- **** Header (.H) File Methods **** -}

--An extremely large function to make the Header File
mkHFile :: Bool -> Maybe String -> CF -> [(Cat,[Rule])] -> String
mkHFile useStl inPackage cf groups = unlines
 [
  printHeader,
  concatMap prDataH groups,
  classFooter,
  showHeader,
  concatMap prDataH groups,
  classFooter,
  footer
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
    "    int len = strlen(s);",
    "    while (cur_ + len >= buf_size)",
    "    {",
    "      buf_size *= 2; /* Double the buffer size */",
    "    }",
    "    resizeBuffer();",
    "    for(int n = 0; n < len; n++)",
    "    {",
    "      buf_[cur_ + n] = s[n];",
    "    }",
    "    cur_ += len;",
    "    buf_[cur_] = 0;",
    "  }",
    "",
    "  void inline bufAppend(const char c)",
    "  {",
    "    if (cur_ >= buf_size)",
    "    {",
    "      buf_size *= 2; /* Double the buffer size */",
    "      resizeBuffer();",
    "    }",
    "    buf_[cur_] = c;",
    "    cur_++;",
    "    buf_[cur_] = 0;",
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
          , "bufAppend(s);"]
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
prDataH :: (Cat, [Rule]) -> String
prDataH (cat, rules) =
 if isList cat
 then concat ["  void visit", cl, "(", cl, " *p);\n"]
 else abstract ++ concatMap prRuleH rules
 where
   cl = identCat (normCat cat)
   abstract = case lookupRule (show cat) rules of
    Just _ -> ""
    Nothing ->  "  void visit" ++ cl ++ "(" ++ cl ++ " *p); /* abstract class */\n"

--Prints all the methods to visit a rule.
prRuleH :: Rule -> String
prRuleH (Rule fun _ _) | isProperLabel fun = concat
  ["  void visit", fun, "(", fun, " *p);\n"]
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
      "  char tmp[16];",
      "  sprintf(tmp, \"%d\", i);",
      "  bufAppend(tmp);",
      "}",
      "",
      "void PrintAbsyn::visitDouble(Double d)",
      "{",
      "  char tmp[16];",
      "  sprintf(tmp, \"%g\", d);",
      "  bufAppend(tmp);",
      "}",
      "",
      "void PrintAbsyn::visitChar(Char c)",
      "{",
      "  bufAppend('\\'');",
      "  bufAppend(c);",
      "  bufAppend('\\'');",
      "}",
      "",
      "void PrintAbsyn::visitString(String s)",
      "{",
      "  bufAppend('\\\"');",
      "  bufAppend(s);",
      "  bufAppend('\\\"');",
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
      "  char tmp[16];",
      "  sprintf(tmp, \"%d\", i);",
      "  bufAppend(tmp);",
      "}",
      "void ShowAbsyn::visitDouble(Double d)",
      "{",
      "  char tmp[16];",
      "  sprintf(tmp, \"%g\", d);",
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

--Generates methods for the Pretty Printer
prPrintData :: Bool -> Maybe String -> CF -> (Cat, [Rule]) -> String
prPrintData True {- use STL -} _ _ (cat@(ListCat _), rules) =
    render $ genPrintVisitorList (cat, rules)
prPrintData False {- use STL -} _ _ (cat@(ListCat _), rules) =
    genPrintVisitorListNoStl (cat, rules)
prPrintData _ inPackage cf (cat, rules) = -- Not a list
 -- a position token
 if isPositionCat cf cat then unlines [
   "void PrintAbsyn::visit" ++ show cat ++ "(" ++ show cat ++ " *p)",
   "{",
   "  visitIdent(p->string_);",
   "}",
   ""
   ]
 else abstract ++ concatMap (prPrintRule inPackage) rules
 where
   cl = identCat (normCat cat)
   abstract = case lookupRule (show cat) rules of
    Just _ -> ""
    Nothing ->  "void PrintAbsyn::visit" ++ cl ++ "(" ++ cl +++ "*p) {} //abstract class\n\n"

-- | Generate pretty printer visitor for a list category:
--
-- >>> let c = Cat "C" ; lc = ListCat c
-- >>> let rules = [Rule "[]" lc [], Rule "(:)" lc [Left c, Right "-", Left lc]]
-- >>> genPrintVisitorList (lc, rules)
-- void PrintAbsyn::visitListC(ListC *listc)
-- {
--   for (ListC::const_iterator i = listc->begin() ; i != listc->end() ; ++i)
--   {
--     (*i)->accept(this);
--     render('-');
--   }
-- }
--
-- >>> let c2 = CoercCat "C" 2 ; lc2 = ListCat c2
-- >>> let rules2 = rules ++ [Rule "[]" lc2 [], Rule "(:)" lc2 [Left c2, Right "+", Left lc2]]
-- >>> genPrintVisitorList (lc, rules2)
-- void PrintAbsyn::visitListC(ListC *listc)
-- {
--   for (ListC::const_iterator i = listc->begin() ; i != listc->end() ; ++i)
--   {
--     (*i)->accept(this);
--     switch(_i_)
--     {
--       case 2: render('+'); break;
--       default: render('-');
--     }
--   }
-- }
genPrintVisitorList :: (Cat, [Rule]) -> Doc
genPrintVisitorList (cat@(ListCat c), rules) =
    "void PrintAbsyn::visit" <> text cl <> "("<> text cl <> " *" <> vname <> ")"
    $$ codeblock 2
      [ "for ("<> text cl <> "::const_iterator i = " <> vname <> "->begin() ; i != " <> vname <> "->end() ; ++i)"
      , codeblock 2
          [ if isTokenCat c
              then "visit" <> text (baseName cl) <> "(*i) ;"
              else "(*i)->accept(this);"
          , (if hasOneFunc rules
              then "if (i != " <> vname <> "->end() - 1)"
              else empty)
            <+> renderListSepByPrecedence "_i_" renderSep separators
          ]
      ]
 where
   separators = getSeparatorByPrecedence rules
   cl = identCat (normCat cat)
   vname = text $ map toLower cl
   renderSep s = "render(" <> text (snd (renderCharOrString s)) <> ")"
genPrintVisitorList _ = error "genPrintVisitorList expects a ListCat"

-- | This is the only part of the pretty printer that differs significantly
-- between the versions with and without STL.
genPrintVisitorListNoStl :: (Cat, [Rule]) -> String
genPrintVisitorListNoStl (cat@(ListCat c), rules) = unlines
    [ "void PrintAbsyn::visit" ++ cl ++ "("++ cl ++ " *" ++ vname ++ ")"
    , "{"
    , "  while(" ++ vname +++ "!= 0)"
    , "  {"
    , "    if (" ++ vname ++ "->" ++ vname ++ "_ == 0)"
    , "    {"
    , visitMember
    , optsep
    , "      " ++ vname +++ "= 0;"
    , "    }"
    , "    else"
    , "    {"
    , visitMember
    , render $ nest 6 $ renderListSepByPrecedence "_i_" renderSep separators
    , "      " ++ vname +++ "=" +++ vname ++ "->" ++ vname ++ "_;"
    , "    }"
    , "  }"
    , "}"
    , ""
    ]
  where
    visitMember = if isTokenCat c
        then "      visit" ++ funName c ++ "(" ++ vname ++ "->" ++ member ++ ");"
        else "      " ++ vname ++ "->" ++ member ++ "->accept(this);"
    cl = identCat (normCat cat)
    ecl = identCat (normCatOfList cat)
    vname = map toLower cl
    member = map toLower ecl ++ "_"
    optsep = if hasOneFunc rules then "" else "      render(" ++ sep ++ ");"
    sep = snd (renderCharOrString sep')
    sep' = getCons rules
    renderSep s = "render(" <> text (snd (renderCharOrString s)) <> ")"
    separators = getSeparatorByPrecedence rules
genPrintVisitorListNoStl _ = error "genPrintVisitorListNoStl expects a ListCat"

--Pretty Printer methods for a rule.
prPrintRule :: Maybe String -> Rule -> String
prPrintRule inPackage r@(Rule fun _ cats) | isProperLabel fun = unlines
  [
   "void PrintAbsyn::visit" ++ fun ++ "(" ++ fun +++ "*" ++ fnm ++ ")",
   "{",
   "  int oldi = _i_;",
   lparen,
   cats',
   rparen,
   "  _i_ = oldi;",
   "}",
   ""
  ]
   where
    p = precRule r
    (lparen, rparen) =
      ("  if (oldi > " ++ show p ++ ") render(" ++ nsDefine inPackage "_L_PAREN" ++ ");\n",
       "  if (oldi > " ++ show p ++ ") render(" ++ nsDefine inPackage "_R_PAREN" ++ ");\n")
    cats' = concatMap (prPrintCat fnm) (numVars cats)
    fnm = "p" --old names could cause conflicts
prPrintRule _ _ = ""

--This goes on to recurse to the instance variables.
prPrintCat :: String -> Either (Cat, Doc) String -> String
prPrintCat _ (Right t) = "  render(" ++ t' ++ ");\n"
  where t' = snd (renderCharOrString t)
prPrintCat fnm (Left (c, nt))
  | isTokenCat c  = "  visit" ++ funName c ++ "(" ++ fnm ++ "->" ++ render nt ++ ");\n"
  | isList c            = "  if(" ++ fnm ++ "->" ++ render nt ++ ") {" ++ accept ++ "}\n"
  | otherwise           = "  " ++ accept ++ "\n"
  where
    accept
      | c == InternalCat =  "/* Internal Category */\n"
      | otherwise        = setI (precCat c) ++ fnm ++ "->" ++ render nt ++ "->accept(this);"

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
    visitMember = if isTokenCat c
      then "      visit" ++ funName c ++ "(" ++ vname ++ "->" ++ member ++ ");"
      else "      " ++ vname ++ "->" ++ member ++ "->accept(this);"
prShowData _ (cat, rules) =  --Not a list:
  abstract ++ concatMap prShowRule rules
  where
    cl = identCat (normCat cat)
    abstract = case lookupRule (show cat) rules of
      Just _ -> ""
      Nothing ->  "void ShowAbsyn::visit" ++ cl ++ "(" ++ cl ++ " *p) {} //abstract class\n\n"

--This prints all the methods for Abstract Syntax tree rules.
prShowRule :: Rule -> String
prShowRule (Rule fun _ cats) | isProperLabel fun = concat
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
    (optspace, lparen, rparen) = if allTerms cats
      then ("","","")
      else ("  bufAppend(' ');\n", "  bufAppend('(');\n","  bufAppend(')');\n")
    cats' = if allTerms cats
        then ""
        else concat (insertSpaces (map (prShowCat fnm) (numVars cats)))
    insertSpaces [] = []
    insertSpaces (x:[]) = [x]
    insertSpaces (x:xs) = if x == ""
      then insertSpaces xs
      else x : "  bufAppend(' ');\n" : insertSpaces xs
    allTerms [] = True
    allTerms (Left _:_) = False
    allTerms (_:zs) = allTerms zs
    fnm = "p" --other names could cause conflicts
prShowRule _ = ""

-- This recurses to the instance variables of a class.
prShowCat :: String -> Either (Cat, Doc) String -> String
prShowCat _ (Right _)               = ""
prShowCat fnm (Left (cat,nt))
  | isTokenCat cat              =
    "  visit" ++ funName cat ++ "(" ++ fnm ++ "->" ++ render nt ++ ");\n"
  | cat == InternalCat                = "/* Internal Category */\n"
  | show (normCat $ strToCat $ render nt) /= render nt = accept
  | otherwise                         =
    concat [
           "  bufAppend('[');\n",
           "  if (" ++ fnm ++ "->" ++ render nt ++ ")" ++ accept,
           "  bufAppend(']');\n"
          ]
  where accept = "  " ++ fnm ++ "->" ++ render nt ++ "->accept(this);\n"

{- **** Helper Functions Section **** -}

-- from ListIdent to Ident
baseName = drop 4


--The visit-function name of a basic type
funName :: Cat -> String
funName (TokenCat c) | c `elem` builtin = c
  where builtin = ["Integer", "Char", "String", "Double", "Ident" ]
funName _ = "Ident" --User-defined type
--The visit-function name of a basic type
-- funName :: String -> String
-- funName v =
--     if "integer_" `isPrefixOf` v then "Integer"
--     else if "char_" `isPrefixOf` v then "Char"
--     else if "string_" `isPrefixOf` v then "String"
--     else if "double_" `isPrefixOf` v then "Double"
--     else if "ident_" `isPrefixOf` v then "Ident"
--     else "Ident" --User-defined type

--Just sets the coercion level for parentheses in the Pretty Printer.
setI :: Integer -> String
setI n = "_i_ = " ++ show n ++ "; "

--An extremely simple renderer for terminals.
prRender :: Bool -> String
prRender useStl = unlines
  [
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
      "  else if (c == 0) return;",
      "  else",
      "  {",
      "     bufAppend(' ');",
      "     bufAppend(c);",
      "     bufAppend(' ');",
      "  }",
      "}",
      "",
      let renderString = "void PrintAbsyn::render(String s_)" $$ codeblock 2
                             [ "const char *s = s_.c_str() ;"
                             , "if(strlen(s) > 0)"
                             , codeblock 2
                                 [ "bufAppend(s);"
                                 , "bufAppend(' ');" ] ]
      in if useStl then render renderString else "",
      "void PrintAbsyn::render(const char *s)",
      "{",
      "  if(strlen(s) > 0)",
      "  {",
      "    bufAppend(s);",
      "    bufAppend(' ');",
      "  }",
      "}",
      "",
      "void PrintAbsyn::indent()",
      "{",
      "  int n = _n_;",
      "  while (n > 0)",
      "  {",
      "    bufAppend(' ');",
      "    n--;",
      "  }",
      "}",
      "",
      "void PrintAbsyn::backup()",
      "{",
      "  if (buf_[cur_ - 1] == ' ')",
      "  {",
      "    buf_[cur_ - 1] = 0;",
      "    cur_--;",
      "  }",
      "}",
      ""
  ]
