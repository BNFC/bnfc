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

module BNFC.Backend.CPP.STL.CFtoSTLPrinter (cf2CPPPrinter) where

import BNFC.CF
import BNFC.Utils ((+++))
import BNFC.Backend.Common.NamedVariables
import BNFC.Backend.Common.StrUtils (renderCharOrString)
import BNFC.Backend.Utils (isTokenType)
import Data.List
import Data.Char(toLower)
import BNFC.Backend.CPP.STL.STLUtils
import Text.PrettyPrint

--Produces (.H file, .C file)
cf2CPPPrinter :: Maybe String -> CF -> (String, String)
cf2CPPPrinter inPackage cf = (mkHFile inPackage cf groups, mkCFile inPackage cf groups)
 where
    groups = positionRules cf ++ (fixCoercions (ruleGroupsInternals cf))

positionRules :: CF -> [(Cat,[Rule])]
positionRules cf =
      [(cat,[Rule (show cat) cat [Left catString, Left catInteger]]) |
        cat <- filter (isPositionCat cf) $ fst (unzip (tokenPragmas cf))]

{- **** Header (.H) File Methods **** -}

--An extremely large function to make the Header File
mkHFile :: Maybe String -> CF -> [(Cat,[Rule])] -> String
mkHFile inPackage cf groups = unlines
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
    "  void render(String s);",
    "  void indent(void);",
    "  void backup(void);",
    " public:",
    "  PrintAbsyn(void);",
    "  ~PrintAbsyn(void);",
    "  char* print(Visitable* v);"
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
    "  void inline bufAppend(const char* s)",
    "  {",
    "    int len = strlen(s);",
    "    while (cur_ + len > buf_size)",
    "    {",
    "      buf_size *= 2; /* Double the buffer size */",
    "      resizeBuffer();",
    "    }",
    "    for(int n = 0; n < len; n++)",
    "    {",
    "      buf_[cur_ + n] = s[n];",
    "    }",
    "    cur_ += len;",
    "    buf_[cur_] = 0;",
    "  }",
    "  void inline bufAppend(const char c)",
    "  {",
    "    if (cur_ == buf_size)",
    "    {",
    "      buf_size *= 2; /* Double the buffer size */",
    "      resizeBuffer();",
    "    }",
    "    buf_[cur_] = c;",
    "    cur_++;",
    "    buf_[cur_] = 0;",
    "  }",
    "  void inline bufReset(void)",
    "  {",
    "    cur_ = 0;",
    "    buf_size = " ++ nsDefine inPackage "BUFFER_INITIAL" ++ ";",
    "    resizeBuffer();",
    "    memset(buf_, 0, buf_size);",
    "  }",
    "  void inline resizeBuffer(void)",
    "  {",
    "    char* temp = (char*) malloc(buf_size);",
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
    "  char *buf_;",
    "  int cur_, buf_size;",
    "};",
    ""
   ]
  showHeader = unlines
   [
    "",
    "class ShowAbsyn : public Visitor",
    "{",
    " public:",
    "  ShowAbsyn(void);",
    "  ~ShowAbsyn(void);",
    "  char* show(Visitable* v);"
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
 then concat ["  void visit", cl, "(", cl, "* p);\n"]
 else abstract ++ (concatMap prRuleH rules)
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
mkCFile :: Maybe String -> CF -> [(Cat,[Rule])] -> String
mkCFile inPackage cf groups = concat
   [
    header,
    nsStart inPackage ++ "\n",
    prRender,
    printEntries,
    concatMap (prPrintData inPackage cf user) groups,
    printBasics,
    printTokens,
    showEntries,
    concatMap (prShowData user) groups,
    showBasics,
    showTokens,
    nsEnd inPackage ++ "\n"
   ]
  where
    user0 = fst (unzip (tokenPragmas cf))
    (_,user) = partition (isPositionCat cf) user0
    header = unlines
     [
      "/*** BNFC-Generated Pretty Printer and Abstract Syntax Viewer ***/",
      "",
      "#include <string>",
      "#include \"Printer.H\"",
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
      "char* PrintAbsyn::print(Visitable *v)",
      "{",
      "  _i_ = 0; _n_ = 0;",
      "  bufReset();",
      "  v->accept(this);",
      "  return buf_;",
      "}"
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
      "char* ShowAbsyn::show(Visitable *v)",
      "{",
      "  bufReset();",
      "  v->accept(this);",
      "  return buf_;",
      "}"
     ]
    printBasics = unlines
     [
      "void PrintAbsyn::visitInteger(Integer i)",
      "{",
      "  char tmp[16];",
      "  sprintf(tmp, \"%d\", i);",
      "  bufAppend(tmp);",
      "}",
      "void PrintAbsyn::visitDouble(Double d)",
      "{",
      "  char tmp[16];",
      "  sprintf(tmp, \"%g\", d);",
      "  bufAppend(tmp);",
      "}",
      "void PrintAbsyn::visitChar(Char c)",
      "{",
      "  bufAppend('\\'');",
      "  bufAppend(c);",
      "  bufAppend('\\'');",
      "}",
      "void PrintAbsyn::visitString(String s_)",
      "{",
      "  const char *s = s_.c_str() ;",
      "  bufAppend('\\\"');",
      "  bufAppend(s);",
      "  bufAppend('\\\"');",
      "}",
      "void PrintAbsyn::visitIdent(String s_)",
      "{",
      "  const char *s = s_.c_str() ;",
      "  render(s);",
      "}",
      ""
     ]

    printTokens = unlines
     [unlines [
      "void PrintAbsyn::visit" ++ t ++ "(String s_)",
      "{",
      "  const char *s = s_.c_str() ;",
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
      "void ShowAbsyn::visitString(String s_)",
      "{",
      "  const char *s = s_.c_str() ;",
      "  bufAppend('\\\"');",
      "  bufAppend(s);",
      "  bufAppend('\\\"');",
      "}",
      "void ShowAbsyn::visitIdent(String s_)",
      "{",
      "  const char *s = s_.c_str() ;",
      "  bufAppend('\\\"');",
      "  bufAppend(s);",
      "  bufAppend('\\\"');",
      "}",
      ""
     ]

    showTokens = unlines
     [unlines [
      "void ShowAbsyn::visit" ++ t ++ "(String s_)",
      "{",
      "  const char *s = s_.c_str() ;",
      "  bufAppend('\\\"');",
      "  bufAppend(s);",
      "  bufAppend('\\\"');",
      "}",
      ""
      ] | t <- tokenNames cf
     ]


{- **** Pretty Printer Methods **** -}

--Generates methods for the Pretty Printer
prPrintData :: Maybe String -> CF -> [UserDef] -> (Cat, [Rule]) -> String
prPrintData _ _ user (cat@(ListCat c), rules) =
 unlines
 [
  "void PrintAbsyn::visit" ++ cl ++ "("++ cl ++ " *" ++ vname ++ ")",
  "{",
  "  for ("++ cl ++"::const_iterator i = " ++
       vname++"->begin() ; i != " ++vname ++"->end() ; ++i)",
  "  {",
  if isTokenType user c
    then "    visit" ++ baseName cl ++ "(*i) ;"
    else "    (*i)->accept(this);",
  optsep,
  "  }",
  "}",
  ""
 ]
 where
   cl = identCat (normCat cat)
   vname = map toLower cl
   sep = snd (renderCharOrString sep')
   sep' = getCons rules
   optsep =
     (if hasOneFunc rules
       then "    if (i != " ++ vname ++ "->end() - 1) "
       else "    "
     )   ++ "render(" ++ sep ++ ");"
prPrintData inPackage cf user (cat, rules) = -- Not a list
 -- a position token
 if isPositionCat cf cat then unlines [
   "void PrintAbsyn::visit" ++ show cat ++ "(" ++ show cat ++ "* p)",
   "{",
   "  visitIdent(p->string_);",
   "}"
   ]
 else abstract ++ (concatMap (prPrintRule inPackage user) rules)
 where
   cl = identCat (normCat cat)
   abstract = case lookupRule (show cat) rules of
    Just _ -> ""
    Nothing ->  "void PrintAbsyn::visit" ++ cl ++ "(" ++ cl ++ "*p) {} //abstract class\n\n"

--Pretty Printer methods for a rule.
prPrintRule :: Maybe String -> [UserDef] -> Rule -> String
prPrintRule inPackage user r@(Rule fun _ cats) | isProperLabel fun = unlines
  [
   "void PrintAbsyn::visit" ++ fun ++ "(" ++ fun ++ "*" +++ fnm ++ ")",
   "{",
   "  int oldi = _i_;",
   lparen,
   cats',
   rparen,
   "  _i_ = oldi;",
   "}\n"
  ]
   where
    p = precRule r
    (lparen, rparen) =
      ("  if (oldi > " ++ (show p) ++ ") render(" ++ nsDefine inPackage "_L_PAREN" ++ ");\n",
       "  if (oldi > " ++ (show p) ++ ") render(" ++ nsDefine inPackage "_R_PAREN" ++ ");\n")
    cats' = concatMap (prPrintCat user fnm) (numVars' cats)
    fnm = "p" --old names could cause conflicts
prPrintRule _ _ _ = ""

--This goes on to recurse to the instance variables.
prPrintCat :: [UserDef] -> String -> Either (Cat, Doc) String -> String
prPrintCat _ _ (Right t) = "  render(" ++ t' ++ ");\n"
  where t' = snd (renderCharOrString t)
prPrintCat user fnm (Left (c, nt))
  | isTokenType user c  = "  visit" ++ funName (render nt) ++ "(" ++ fnm ++ "->" ++ render nt ++ ");\n"
  | isList c            = "  if(" ++ fnm ++ "->" ++ render nt ++ ") {" ++ accept ++ "}"
  | otherwise           = "  " ++ accept ++ "\n"
  where
    accept
      | c == InternalCat =  "/* Internal Category */\n"
      | otherwise        = setI (precCat c) ++ fnm ++ "->" ++ render nt ++ "->accept(this);"

{- **** Abstract Syntax Tree Printer **** -}

--This prints the functions for Abstract Syntax tree printing.
prShowData :: [UserDef] -> (Cat, [Rule]) -> String
prShowData user (cat@(ListCat c), _) = unlines
 [
  "void ShowAbsyn::visit" ++ cl ++ "("++ cl ++ " *" ++ vname ++ ")",
  "{",
  "  for ("++ cl ++"::const_iterator i = " ++
       vname++"->begin() ; i != " ++vname ++"->end() ; ++i)",
  "  {",
  if isTokenType user c
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
prShowData user (cat, rules) =  --Not a list:
  abstract ++ (concatMap (prShowRule user) rules)
  where
    cl = identCat (normCat cat)
    abstract = case lookupRule (show cat) rules of
      Just _ -> ""
      Nothing ->  "void ShowAbsyn::visit" ++ cl ++ "(" ++ cl ++ "* p) {} //abstract class\n\n"

--This prints all the methods for Abstract Syntax tree rules.
prShowRule :: [UserDef] -> Rule -> String
prShowRule user (Rule fun _ cats) | isProperLabel fun = concat
  [
   "void ShowAbsyn::visit" ++ fun ++ "(" ++ fun ++ "*" +++ fnm ++ ")\n",
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
    	else concat (insertSpaces (map (prShowCat user fnm) (numVars' cats)))
    insertSpaces [] = []
    insertSpaces (x:[]) = [x]
    insertSpaces (x:xs) = if x == ""
      then insertSpaces xs
      else (x : ["  bufAppend(' ');\n"]) ++ (insertSpaces xs)
    allTerms [] = True
    allTerms (Left _:_) = False
    allTerms (_:zs) = allTerms zs
    fnm = "p" --other names could cause conflicts
prShowRule _ _ = ""

--This recurses to the instance variables of a class.
prShowCat :: [UserDef] -> String -> Either (Cat, Doc) String -> String
prShowCat _ _ (Right _)               = ""
prShowCat user fnm (Left (cat,nt))
  | isTokenType user cat              =
    "  visit" ++ funName (render nt) ++ "(" ++ fnm ++ "->" ++ render nt ++ ");\n"
  | cat == InternalCat                = "/* Internal Category */\n"
  | (show $normCat$strToCat$render nt) /= render nt = accept
  | otherwise                         =
    concat [
	   "  bufAppend('[');\n",
	   "  if (" ++ fnm ++ "->" ++ render nt ++ ")" ++ accept,
	   "  bufAppend(']');\n"
	  ]
  where accept = "  " ++ fnm ++ "->" ++ render nt ++ "->accept(this);\n"

{- **** Helper Functions Section **** -}

-- from ListIdent to Ident
baseName cl = drop 4 cl


--The visit-function name of a basic type
funName :: String -> String
funName v =
    if "integer_" `isPrefixOf` v then "Integer"
    else if "char_" `isPrefixOf` v then "Char"
    else if "string_" `isPrefixOf` v then "String"
    else if "double_" `isPrefixOf` v then "Double"
    else if "ident_" `isPrefixOf` v then "Ident"
    else "Ident" --User-defined type

--Just sets the coercion level for parentheses in the Pretty Printer.
setI :: Integer -> String
setI n = "_i_ = " ++ (show n) ++ "; "

--An extremely simple renderer for terminals.
prRender :: String
prRender = unlines
  [
      "//You may wish to change render",
      "void PrintAbsyn::render(Char c)",
      "{",
      "  if (c == '{')",
      "  {",
      "     bufAppend('\\n');",
      "     indent();",
      "     bufAppend(c);",
      "     _n_ = _n_ + 2;",
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
      "     _n_ = _n_ - 2;",
      "     backup();",
      "     backup();",
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
      "     bufAppend(c);",
      "     bufAppend(' ');",
      "  }",
      "}",
      "void PrintAbsyn::render(String s_)",
      "{",
      "  const char *s = s_.c_str() ;",
      "  if(strlen(s) > 0)",
      "  {",
      "    bufAppend(s);",
      "    bufAppend(' ');",
      "  }",
      "}",
      "void PrintAbsyn::indent()",
      "{",
      "  int n = _n_;",
      "  while (n > 0)",
      "  {",
      "    bufAppend(' ');",
      "    n--;",
      "  }",
      "}",
      "void PrintAbsyn::backup()",
      "{",
      "  if (buf_[cur_ - 1] == ' ')",
      "  {",
      "    buf_[cur_ - 1] = 0;",
      "    cur_--;",
      "  }",
      "}"
  ]
