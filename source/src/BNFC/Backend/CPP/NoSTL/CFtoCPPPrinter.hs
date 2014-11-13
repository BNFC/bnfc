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

module BNFC.Backend.CPP.NoSTL.CFtoCPPPrinter (cf2CPPPrinter) where

import BNFC.CF
import BNFC.Utils ((+++))
import BNFC.Backend.Common.NamedVariables
import BNFC.Backend.Common.StrUtils (renderCharOrString)
import BNFC.Backend.Utils (isTokenType)
import Data.Char(toLower)
import Text.PrettyPrint

--Produces (.H file, .C file)
cf2CPPPrinter :: CF -> (String, String)
cf2CPPPrinter cf = (mkHFile cf groups, mkCFile cf groups)
 where
    groups = fixCoercions (ruleGroupsInternals cf)

{- **** Header (.H) File Methods **** -}

--An extremely large function to make the Header File
mkHFile :: CF -> [(Cat,[Rule])] -> String
mkHFile _ groups = unlines
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
prPrintData user (cat@(ListCat c), rules) =
 unlines
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
 ]
  where
    visitMember = if isTokenType user c
        then "      visit" ++ funName c ++ "(" ++ vname ++ "->" ++ member ++ ");"
        else "      " ++ vname ++ "->" ++ member ++ "->accept(this);"
    cl = identCat (normCat cat)
    ecl = identCat (normCatOfList cat)
    vname = map toLower cl
    member = map toLower ecl ++ "_"
    optsep = if hasOneFunc rules then "" else ("      render(" ++ sep ++ ");")
    sep = snd (renderCharOrString sep')
    sep' = getCons rules
prPrintData user (cat, rules) = --Not a list:
    abstract ++ concatMap (render . prPrintRule user) rules
  where
    cl = identCat (normCat cat)
    abstract = case lookupRule (show cat) rules of
      Just _ -> ""
      Nothing ->  "void PrintAbsyn::visit" ++ cl ++ "(" ++ cl ++ "*p) {} //abstract class\n\n"

-- | Pretty Printer methods for a rule.
-- >>> prPrintRule [Cat "A"] (Rule "Myf" (Cat "X") [Left (Cat "A"), Left (Cat "Integer"), Right "abc", Left (Cat "B")])
-- void PrintAbsyn::visitMyf(Myf* p)
-- { int oldi = _i_;
--   if (oldi > 0) render(_L_PAREN);
--   visitIdent(p->a_);
--   visitInteger(p->integer_);
--   render("abc");
--   _i_ = 0; p->b_->accept(this);
--   if (oldi > 0) render(_R_PAREN);
--   _i_ = oldi;
-- }
prPrintRule :: [UserDef] -> Rule -> Doc
prPrintRule user r@(Rule fun _ cats) | isProperLabel fun = vcat
    [ "void PrintAbsyn::visit" <> text fun <> parens (text fun <> "*" <+>  fnm)
    , "{"
    , nest 2 (vcat
        [ "int oldi = _i_;"
        , "if (oldi > " <> integer p <> ") render(_L_PAREN);"
        , vcat cats'
        , "if" <+> parens ("oldi >" <+> integer p) <+> "render(_R_PAREN);"
        , "_i_ = oldi;" ])
    , "}"
    ]
   where
    p = precRule r
    cats' = map (prPrintCat user fnm) (numVars' cats)
    fnm = "p" --old names could cause conflicts
prPrintRule _ _ = ""

-- | This goes on to recurse to the instance variables.
-- >>> prPrintCat [] "MyFun" (Left (Cat "A", "a_"))
-- _i_ = 0; MyFun->a_->accept(this);
-- >>> prPrintCat [] "MyFun" (Left (CoercCat "Exp" 2, "exp_"))
-- _i_ = 2; MyFun->exp_->accept(this);
-- >>> prPrintCat [] "MyFun" (Right "abc")
-- render("abc");
-- >>> prPrintCat [Cat "A"] "MyFun" (Left (Cat "A", "a_"))
-- visitIdent(MyFun->a_);
-- >>> prPrintCat [] "MyFun" (Left (Cat "Integer", "integer_"))
-- visitInteger(MyFun->integer_);
prPrintCat :: [UserDef] -> Doc -> Either (Cat, Doc) String -> Doc
prPrintCat _ _ (Right t) =
    "render" <> parens (text (snd (renderCharOrString t))) <> ";"
prPrintCat user fnm (Left (cat, nt)) | isTokenType user cat =
    "visit" <> text (funName cat) <> "(" <> fnm <> "->" <> nt <> ");"
prPrintCat _ fnm (Left (cat, nt)) =
    if isList cat
    then "if(" <> fnm <> "->" <> nt <> ")" <+> braces accept
    else accept
  where
      accept = if cat == InternalCat
               then "/* Internal Category */"
               else setI (precCat cat) <+> fnm <> "->" <> nt <> "->accept(this);"
      -- Just sets the coercion level for parentheses in the Pretty Printer.
      setI :: Integer -> Doc
      setI n = "_i_ =" <+> integer n <> ";"


{- **** Abstract Syntax Tree Printer **** -}

--This prints the functions for Abstract Syntax tree printing.
prShowData :: [UserDef] -> (Cat, [Rule]) -> String
prShowData user (cat@(ListCat c), rules) =
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
    visitMember = if isTokenType user c
      then "      visit" ++ funName c ++ "(" ++ vname ++ "->" ++ member ++ ");"
      else "      " ++ vname ++ "->" ++ member ++ "->accept(this);"
prShowData user (cat, rules) = --Not a list:
    abstract ++ concatMap (prShowRule user) rules
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
      else (x : ["  bufAppend(' ');\n"]) ++ insertSpaces xs
    allTerms [] = True
    allTerms (Left _:_) = False
    allTerms (_:zs) = allTerms zs
    fnm = "p" --other names could cause conflicts
prShowRule _ _ = ""

--This recurses to the instance variables of a class.
prShowCat :: [UserDef] -> String -> Either (Cat, Doc) String -> String
prShowCat user fnm c =
  case c of
    Right _ -> ""
    Left (cat, nt) ->
      if isTokenType user cat
       then "  visit" ++ funName cat ++ "(" ++ fnm ++ "->" ++ render nt ++ ");\n"
       else if cat == InternalCat
       then "/* Internal Category */\n"
       else if ((show$normCat$strToCat$render nt) /= render nt)
          then accept
 	  else concat
	  [
	   "  bufAppend('[');\n",
	   "  if (" ++ fnm ++ "->" ++ render nt ++ ")" ++ accept,
	   "  bufAppend(']');\n"
	  ]
       where
         accept = "  " ++ fnm ++ "->" ++ render nt ++ "->accept(this);\n"

{- **** Helper Functions Section **** -}

--The visit-function name of a basic type
funName :: Cat -> String
funName (Cat c) | c `elem` builtin = c
  where builtin = ["Integer", "Char", "String", "Double", "Ident" ]
funName _ = "Ident" --User-defined type


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
