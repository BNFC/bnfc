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

module CFtoCPPPrinter (cf2CPPPrinter) where

import CF
import Utils ((+++), (++++))
import NamedVariables
import Data.List
import Data.Char(toLower, toUpper)

--Produces (.H file, .C file)
cf2CPPPrinter :: CF -> (String, String)
cf2CPPPrinter cf = (mkHFile cf groups, mkCFile cf groups)
 where
    groups = (fixCoercions (ruleGroupsInternals cf))

{- **** Header (.H) File Methods **** -}

--An extremely large function to make the Header File
mkHFile :: CF -> [(Cat,[Rule])] -> String
mkHFile cf groups = unlines
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
    "#ifndef PRINTER_HEADER",
    "#define PRINTER_HEADER",
    "",
    "#include \"Absyn.H\"",
    "#include <stdio.h>",
    "#include <string.h>",
    "#include <stdlib.h>",
    "",
    "/* Certain applications may improve performance by changing the buffer size */",
    "#define BUFFER_INITIAL 2000",
    "/* You may wish to change _L_PAREN or _R_PAREN */",
    "#define _L_PAREN '('",
    "#define _R_PAREN ')'",
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
  classFooter = unlines
   [
    "  void visitInteger(Integer i);",
    "  void visitDouble(Double d);",
    "  void visitChar(Char c);",
    "  void visitString(String s);",
    "  void visitIdent(String s);",
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
    "    buf_size = BUFFER_INITIAL;",
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
    "",
    "#endif"
   ]

--Prints all the required method names and their parameters.
prDataH :: (Cat, [Rule]) -> String
prDataH (cat, rules) =
 if "List" `isPrefixOf` (identCat cat)
 then concat ["  void visit", cl, "(", cl, "* p);\n"]
 else abstract ++ (concatMap prRuleH rules)
 where
   cl = identCat (normCat cat)
   abstract = case lookup cat rules of
    Just x -> ""
    Nothing ->  "  void visit" ++ cl ++ "(" ++ cl ++ " *p); /* abstract class */\n"

--Prints all the methods to visit a rule.
prRuleH :: Rule -> String
prRuleH (fun, (c, cats)) | isProperLabel fun = concat
  ["  void visit", fun, "(", fun, " *p);\n"]
prRuleH (fun, cats) = ""

{- **** Implementation (.C) File Methods **** -}

--This makes the .C file by a similar method.
mkCFile :: CF -> [(Cat,[Rule])] -> String
mkCFile cf groups = concat
   [
    header,
    prRender,
    printEntries,
    concatMap (prPrintData user) groups,
    printBasics,
    showEntries,
    concatMap (prShowData user) groups,
    showBasics
   ]
  where
    user = fst (unzip (tokenPragmas cf))
    header = unlines
     [
      "/*** BNFC-Generated Pretty Printer and Abstract Syntax Viewer ***/",
      "",
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
      "void PrintAbsyn::visitString(String s)",
      "{",
      "  bufAppend('\\\"');",
      "  bufAppend(s);",
      "  bufAppend('\\\"');",
      "}",
      "void PrintAbsyn::visitIdent(String s)",
      "{",
      "  render(s);",
      "}",
      ""
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

{- **** Pretty Printer Methods **** -}

--Generates methods for the Pretty Printer
prPrintData :: [UserDef] -> (Cat, [Rule]) -> String
prPrintData user (cat, rules) =
 if "List" `isPrefixOf` (identCat cat)
 then unlines
 [
  "void PrintAbsyn::visit" ++ cl ++ "("++ cl ++ " *" ++ vname ++ ")",
  "{",
  "  while(" ++ vname ++ "!= 0)",
  "  {",
  "    if (" ++ vname ++ "->" ++ vname ++ "_ == 0)",
  "    {",
  visitMember,
  optsep,
  "      " ++ vname +++ "= 0;",
  "    }",
  "    else",
  "    {",
  visitMember,
  "      render(" ++ sep ++ ");",
  "      " ++ vname +++ "=" +++ vname ++ "->" ++ vname ++ "_;",
  "    }",
  "  }",
  "}",
  ""
 ] --Not a list:
 else abstract ++ (concatMap (prPrintRule user) rules)
 where
   cl = identCat (normCat cat)
   ecl = identCat (normCatOfList cat)
   vname = map toLower cl
   member = map toLower ecl ++ "_"
   visitMember = if isBasic user member
     then "      visit" ++ (funName member) ++ "(" ++ vname ++ "->" ++ member ++ ");"
     else "      " ++ vname ++ "->" ++ member ++ "->accept(this);"
   sep = if (length sep') == 1
     then "'" ++ (escapeChars sep') ++ "'"
     else "\"" ++ (escapeChars sep') ++ "\""
   sep' = getCons rules
   optsep = if hasOneFunc rules then "" else ("      render(" ++ sep ++ ");")
   abstract = case lookup cat rules of
    Just x -> ""
    Nothing ->  "void PrintAbsyn::visit" ++ cl ++ "(" ++ cl ++ "*p) {} //abstract class\n\n"

--Pretty Printer methods for a rule.
prPrintRule :: [UserDef] -> Rule -> String
prPrintRule user r@(fun, (c, cats)) | isProperLabel fun = unlines
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
      ("  if (oldi > " ++ (show p) ++ ") render(_L_PAREN);\n",
       "  if (oldi > " ++ (show p) ++ ") render(_R_PAREN);\n")
    cats' = (concatMap (prPrintCat user fnm) (zip (fixOnes (numVars [] cats)) (map getPrec cats)))
    fnm = "p" --old names could cause conflicts
    getPrec (Right s) = 0
    getPrec (Left c) = precCat c
prPrintRule _ (fun, cats) = ""

--This goes on to recurse to the instance variables.
prPrintCat :: [UserDef] -> String -> (Either Cat String, Int) -> String
prPrintCat user fnm (c,p) = case c of
  (Right t) -> "  render(" ++ t' ++ ");\n"
    where
     t' = if length t == 1
       then "'" ++ (escapeChars t) ++ "'"
       else "\"" ++ (escapeChars t) ++ "\""
  (Left nt) -> if isBasic user nt
       then "  visit" ++ (funName nt) ++ "(" ++ fnm ++ "->" ++ nt ++ ");\n"
       else if "list" `isPrefixOf` nt
         then "  if(" ++ fnm ++ "->" ++ nt ++ ") {" ++ accept ++ "}"
	 else "  " ++ accept ++ "\n"
     where
       accept = if nt == "#_" --Internal category
         then "/* Internal Category */\n"
         else (setI p) ++ fnm ++ "->" ++ nt ++ "->accept(this);"

{- **** Abstract Syntax Tree Printer **** -}

--This prints the functions for Abstract Syntax tree printing.
prShowData :: [UserDef] -> (Cat, [Rule]) -> String
prShowData user (cat, rules) =
 if "List" `isPrefixOf` (identCat cat)
 then unlines
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
 ] --Not a list:
 else abstract ++ (concatMap (prShowRule user) rules)
 where
   cl = identCat (normCat cat)
   ecl = identCat (normCatOfList cat)
   vname = map toLower cl
   member = map toLower ecl ++ "_"
   visitMember = if isBasic user member
     then "      visit" ++ (funName member) ++ "(" ++ vname ++ "->" ++ member ++ ");"
     else "      " ++ vname ++ "->" ++ member ++ "->accept(this);"
   abstract = case lookup cat rules of
    Just x -> ""
    Nothing ->  "void ShowAbsyn::visit" ++ cl ++ "(" ++ cl ++ "* p) {} //abstract class\n\n"

--This prints all the methods for Abstract Syntax tree rules.
prShowRule :: [UserDef] -> Rule -> String
prShowRule user (fun, (c, cats)) | isProperLabel fun = concat
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
    	else concat (insertSpaces (map (prShowCat user fnm) (fixOnes (numVars [] cats))))
    insertSpaces [] = []
    insertSpaces (x:[]) = [x]
    insertSpaces (x:xs) = if x == ""
      then insertSpaces xs
      else (x : ["  bufAppend(' ');\n"]) ++ (insertSpaces xs)
    allTerms [] = True
    allTerms ((Left z):zs) = False
    allTerms (z:zs) = allTerms zs
    fnm = "p" --other names could cause conflicts
prShowRule _ (fun, cats) = ""

--This recurses to the instance variables of a class.
prShowCat :: [UserDef] -> String -> Either Cat String -> String
prShowCat user fnm c =
  case c of
    (Right t) -> ""
    (Left nt) ->
      if isBasic user nt
       then "  visit" ++ (funName nt) ++ "(" ++ fnm ++ "->" ++ nt ++ ");\n"
       else if nt == "#_" --internal category
       then "/* Internal Category */\n"
       else if ((normCat nt) /= nt)
          then accept
 	  else concat
	  [
	   "  bufAppend('[');\n",
	   "  if (" ++ fnm ++ "->" ++ nt ++ ")" ++ accept,
	   "  bufAppend(']');\n"
	  ]
       where
         accept = "  " ++ fnm ++ "->" ++ nt ++ "->accept(this);\n"

{- **** Helper Functions Section **** -}


--Just checks if something is a basic or user-defined type.
--This is because you don't -> a basic non-pointer type.
isBasic :: [UserDef] -> String -> Bool
isBasic user v =
  if elem (init v) user'
    then True
    else if "integer_" `isPrefixOf` v then True
    else if "char_" `isPrefixOf` v then True
    else if "string_" `isPrefixOf` v then True
    else if "double_" `isPrefixOf` v then True
    else if "ident_" `isPrefixOf` v then True
    else False
  where
   user' = map (map toLower) user

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
setI :: Int -> String
setI n = "_i_ = " ++ (show n) ++ "; "

--Gets the separator for a list.
getCons :: [Rule] -> String
getCons ((f, (c, cats)):rs) =
 if isConsFun f
   then seper cats
   else getCons rs
 where
    seper [] = []
    seper ((Right x):xs) = x
    seper ((Left x):xs) = seper xs

--Checks if the list has a non-empty rule.
hasOneFunc :: [Rule] -> Bool
hasOneFunc [] = False
hasOneFunc ((f, (c, cats)):rs) =
 if (isOneFun f)
    then True
    else hasOneFunc rs

--Helper function that escapes characters in strings
escapeChars :: String -> String
escapeChars [] = []
escapeChars ('\\':xs) = '\\' : ('\\' : (escapeChars xs))
escapeChars ('\"':xs) = '\\' : ('\"' : (escapeChars xs))
escapeChars (x:xs) = x : (escapeChars xs)

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
      "void PrintAbsyn::render(String s)",
      "{",
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
