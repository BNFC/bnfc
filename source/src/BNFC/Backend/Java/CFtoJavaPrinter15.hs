{-# LANGUAGE NoImplicitPrelude #-}

{-
    BNF Converter: Java Pretty Printer generator
    Copyright (C) 2004  Author:  Michael Pellauer, Bjorn Bringert

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1335, USA
-}


{-
   **************************************************************
    BNF Converter Module

    Description   : This module generates the Java Pretty Printer
                    class. In addition, since there's no good way
                    to display a class heirarchy (toString() doesn't
                    count) in Java, it generates a method that
                    displays the Abstract Syntax in a way similar
                    to Haskell.

                    This uses Appel's method and may serve as a
                    useful example to those who wish to use it.

    Author        : Michael Pellauer (pellauer@cs.chalmers.se),
                    Bjorn Bringert (bringert@cs.chalmers.se)

    License       : GPL (GNU General Public License)

    Created       : 24 April, 2003

    Modified      : 9 Aug, 2004

    Added string buffer for efficiency (Michael, August 03)
   **************************************************************
-}
module BNFC.Backend.Java.CFtoJavaPrinter15 ( cf2JavaPrinter ) where

import Prelude'

import BNFC.Backend.Java.CFtoJavaAbs15

import BNFC.CF
import BNFC.Backend.Common (renderListSepByPrecedence)
import BNFC.Backend.Common.NamedVariables
import BNFC.Utils ( (+++) )
import Data.List
import Data.Char ( toLower, isSpace )
import Data.Either (lefts)
import BNFC.PrettyPrint

--Produces the PrettyPrinter class.
--It will generate two methods "print" and "show"
--print is the actual pretty printer for linearization.
--show produces a Haskell-style syntax that can be extremely useful
--especially for testing parser correctness.

cf2JavaPrinter :: String -> String -> CF -> String
cf2JavaPrinter packageBase packageAbsyn cf =
  unlines
   [
    header,
    prEntryPoints packageAbsyn cf,
    unlines (map (prData packageAbsyn user) groups),
    unlines (map (shData packageAbsyn user) groups),
    footer
   ]
  where
    user = [n | (n,_) <- tokenPragmas cf]
    groups = fixCoercions (ruleGroupsInternals cf)
    header = unlines [
      "package" +++ packageBase ++ ";",
      "import" +++ packageAbsyn ++ ".*;",
      "",
      "public class PrettyPrinter",
      "{",
      "  //For certain applications increasing the initial size of the buffer may improve performance.",
      "  private static final int INITIAL_BUFFER_SIZE = 128;",
      "  private static final int INDENT_WIDTH = 2;",
      "  //You may wish to change the parentheses used in precedence.",
      "  private static final String _L_PAREN = new String(\"(\");",
      "  private static final String _R_PAREN = new String(\")\");",
      prRender
      ]
    footer = unlines [ --later only include used categories
      "  private static void pp(Integer n, int _i_) { buf_.append(n); buf_.append(\" \"); }",
      "  private static void pp(Double d, int _i_) { buf_.append(d); buf_.append(\" \"); }",
      "  private static void pp(String s, int _i_) { buf_.append(s); buf_.append(\" \"); }",
      "  private static void pp(Character c, int _i_) { buf_.append(\"'\" + c.toString() + \"'\"); buf_.append(\" \"); }",
      "  private static void sh(Integer n) { render(n.toString()); }",
      "  private static void sh(Double d) { render(d.toString()); }",
      "  private static void sh(Character c) { render(c.toString()); }",
      "  private static void sh(String s) { printQuoted(s); }",
      "  private static void printQuoted(String s) { render(\"\\\"\" + s + \"\\\"\"); }",
      "  private static void indent()",
      "  {",
      "    int n = _n_;",
      "    while (n > 0)",
      "    {",
      "      buf_.append(\" \");",
      "      n--;",
      "    }",
      "  }",
      "  private static void backup()",
      "  {",
      "     if (buf_.charAt(buf_.length() - 1) == ' ') {",
      "      buf_.setLength(buf_.length() - 1);",
      "    }",
      "  }",
      "  private static void trim()",
      "  {",
      "     while (buf_.length() > 0 && buf_.charAt(0) == ' ')",
      "        buf_.deleteCharAt(0); ",
      "    while (buf_.length() > 0 && buf_.charAt(buf_.length()-1) == ' ')",
      "        buf_.deleteCharAt(buf_.length()-1);",
      "  }",
      "  private static int _n_ = 0;",
      "  private static StringBuilder buf_ = new StringBuilder(INITIAL_BUFFER_SIZE);",
      "}"
      ]

--An extremely simple renderer for terminals.
prRender :: String
prRender = unlines
  [
      "  //You may wish to change render",
      "  private static void render(String s)",
      "  {",
      "    if (s.equals(\"{\"))",
      "    {",
      "       buf_.append(\"\\n\");",
      "       indent();",
      "       buf_.append(s);",
      "       _n_ = _n_ + INDENT_WIDTH;",
      "       buf_.append(\"\\n\");",
      "       indent();",
      "    }",
      "    else if (s.equals(\"(\") || s.equals(\"[\"))",
      "       buf_.append(s);",
      "    else if (s.equals(\")\") || s.equals(\"]\"))",
      "    {",
      "       backup();",
      "       buf_.append(s);",
      "       buf_.append(\" \");",
      "    }",
      "    else if (s.equals(\"}\"))",
      "    {",
      "       int t;",
      "       _n_ = _n_ - INDENT_WIDTH;",
      "       for(t=0; t<INDENT_WIDTH; t++) {",
      "         backup();",
      "       }",
      "       buf_.append(s);",
      "       buf_.append(\"\\n\");",
      "       indent();",
      "    }",
      "    else if (s.equals(\",\"))",
      "    {",
      "       backup();",
      "       buf_.append(s);",
      "       buf_.append(\" \");",
      "    }",
      "    else if (s.equals(\";\"))",
      "    {",
      "       backup();",
      "       buf_.append(s);",
      "       buf_.append(\"\\n\");",
      "       indent();",
      "    }",
      "    else if (s.equals(\"\")) return;",
      "    else",
      "    {",
      "       buf_.append(s);",
      "       buf_.append(\" \");",
      "    }",
      "  }"
  ]

prEntryPoints :: String -> CF -> String
prEntryPoints packageAbsyn cf =
    msg ++ concatMap prEntryPoint (allCats cf) ++ msg2
 where
  msg = "  //  print and show methods are defined for each category.\n"
  msg2 = "  /***   You shouldn't need to change anything beyond this point.   ***/\n"
  prEntryPoint cat | normCat cat == cat = unlines
   [
    "  public static String print(" ++ packageAbsyn ++ "." ++ cat' ++ " foo)",
    "  {",
    "    pp(foo, 0);",
    "    trim();",
    "    String temp = buf_.toString();",
    "    buf_.delete(0,buf_.length());",
    "    return temp;",
    "  }",
    "  public static String show(" ++ packageAbsyn ++ "." ++ cat' ++ " foo)",
    "  {",
    "    sh(foo);",
    "    String temp = buf_.toString();",
    "    buf_.delete(0,buf_.length());",
    "    return temp;",
    "  }"
   ]
   where
    cat' = identCat cat
  prEntryPoint _ = ""

prData :: String ->  [UserDef] -> (Cat, [Rule]) -> String
prData packageAbsyn user (cat, rules) = unlines k
    where
      k = if isList cat
           then
           ["  private static void pp(" ++ packageAbsyn ++ "."
                ++ identCat (normCat cat) +++ "foo, int _i_)"
            , "  {"
            , render $ nest 5 $ prList user cat rules <> "  }"
           ]
           else --not a list
           [
            "  private static void pp(" ++ packageAbsyn ++ "."
                ++ identCat (normCat cat) +++ "foo, int _i_)",
            "  {",
            concat (addElse $ map (prRule packageAbsyn) rules) ++ "  }"
           ]
      addElse = map ("    "++). intersperse "else " . filter (not . null)
        . map (dropWhile isSpace)


prRule :: String -> Rule -> String
prRule packageAbsyn r@(Rule fun _c cats) | not (isCoercion fun || isDefinedRule fun) = concat
    [ "    if (foo instanceof" +++ packageAbsyn ++ "." ++ fun ++ ")\n"
    , "    {\n"
    , "       " ++ packageAbsyn ++ "." ++ fun +++ fnm +++ "= ("
        ++ packageAbsyn ++ "." ++ fun ++ ") foo;\n"
    , lparen
    , cats'
    , rparen
    , "    }\n"
    ]
  where
    p = precRule r
    (lparen, rparen) =
        ("       if (_i_ > " ++ show p ++ ") render(_L_PAREN);\n",
        "       if (_i_ > " ++ show p ++ ") render(_R_PAREN);\n")
    cats' = case cats of
        [] -> ""
        _  -> concatMap (render . prCat (text fnm)) (numVars cats)
    fnm = '_' : map toLower fun

prRule _nm _ = ""

-- |
--
-- >>> let lfoo = ListCat (Cat "Foo")
-- >>> prList [] lfoo [Rule "[]" lfoo [], Rule "(:)" lfoo [Left (Cat "Foo"), Right ".", Left lfoo]]
-- for (java.util.Iterator<Foo> it = foo.iterator(); it.hasNext();)
-- {
--   pp(it.next(), _i_);
--   if (it.hasNext()) {
--     render(".");
--   } else {
--     render(".");
--   }
-- }

prList :: [UserDef] -> Cat -> [Rule] -> Doc
prList user c rules =
    "for (java.util.Iterator<" <> et <> "> it = foo.iterator(); it.hasNext();)"
    $$ codeblock 2
        [ "pp(it.next(), _i_);"
        , "if (it.hasNext()) {"
        , nest 2 (renderListSepByPrecedence "_i_" renderSep
            (getSeparatorByPrecedence rules))
        , "} else {"
        , nest 2 (renderSep optsep <> ";")
        , "}"
        ]
   where
    et = text $ cat2JavaType user $ normCatOfList c
    sep = escapeChars $ getCons rules
    optsep = if hasOneFunc rules then "" else sep
    renderSep x = "render(\"" <> text x <>"\")"

-- |
-- >>> prCat "F" (Right "++")
--        render("++");
-- <BLANKLINE>
-- >>> prCat "F" (Left (TokenCat "String", "string_"))
--        printQuoted(F.string_);
-- <BLANKLINE>
-- >>> prCat "F" (Left (InternalCat, "#_"))
-- <BLANKLINE>
-- >>> prCat "F" (Left (Cat "Abc", "abc_"))
--        pp(F.abc_, 0);
-- <BLANKLINE>
prCat :: Doc -> Either (Cat, Doc) String -> Doc
prCat _ (Right t) = nest 7 ("render(\"" <> text(escapeChars t) <> "\");\n")
prCat fnm (Left (TokenCat "String", nt))
    = nest 7 ("printQuoted(" <> fnm <> "." <> nt <> ");\n")
prCat _ (Left (InternalCat, _)) = empty
prCat fnm (Left (cat, nt))
    = nest 7 ("pp(" <> fnm <> "." <> nt <> ", " <> integer (precCat cat) <> ");\n")

--The following methods generate the Show function.

shData :: String -> [UserDef] -> (Cat, [Rule]) -> String
shData packageAbsyn user (cat, rules) = unlines k
    where
      k = if isList cat
          then
          [ "  private static void sh(" ++ packageAbsyn ++ "."
                ++ identCat (normCat cat) +++ "foo)"
          , "  {"
          , shList user cat rules ++ "  }"
          ]
          else
          [ "  private static void sh(" ++ packageAbsyn ++ "."
                ++ identCat (normCat cat) +++ "foo)"
          , "  {"
          , concatMap (shRule packageAbsyn) rules ++ "  }"
          ]


shRule :: String -> Rule -> String
shRule packageAbsyn (Rule fun _c cats) | not (isCoercion fun || isDefinedRule fun) = unlines
    [ "    if (foo instanceof" +++ packageAbsyn ++ "." ++ fun ++ ")"
    , "    {"
    , "       " ++ packageAbsyn ++ "." ++ fun +++ fnm +++ "= ("
        ++ packageAbsyn ++ "." ++ fun ++ ") foo;"
    , members ++ "    }"
    ]
  where
    members = concat [ lparen
                     , "       render(\"" ++ escapeChars fun ++ "\");\n"
                     , cats'
                     , rparen ]
    cats' = if allTerms cats
        then ""
        else concatMap (render . shCat (text fnm)) (lefts (numVars cats))
    (lparen, rparen) = if allTerms cats
        then ("","")
        else ("       render(\"(\");\n","       render(\")\");\n")
    allTerms [] = True
    allTerms ((Left {}):_) = False
    allTerms (_:zs) = allTerms zs
    fnm = '_' : map toLower fun
shRule _nm _ = ""

shList :: [UserDef] -> Cat -> [Rule] -> String
shList user c _rules = unlines
  [
   "     for (java.util.Iterator<" ++ et
          ++ "> it = foo.iterator(); it.hasNext();)",
   "     {",
   "       sh(it.next());",
   "       if (it.hasNext())",
   "         render(\",\");",
   "     }"
  ]
    where
    et = cat2JavaType user $ normCatOfList c

-- |
-- >>> shCat "F" (ListCat (Cat "A"), "lista_")
--        render("[");
--        sh(F.lista_);
--        render("]");
-- <BLANKLINE>
-- >>> shCat "F" (InternalCat, "#_")
-- <BLANKLINE>
-- >>> shCat "F" (Cat "A", "a_")
--        sh(F.a_);
-- <BLANKLINE>
shCat :: Doc -> (Cat, Doc) -> Doc
shCat fnm (ListCat _, vname) = vcat
    [ "       render(\"[\");"
    , "       sh(" <> fnm <> "." <> vname <> ");"
    , "       render(\"]\");\n" ]
shCat _ (InternalCat, _)     = empty
shCat fname (_, vname)       = "       sh(" <> fname <> "." <> vname <> ");\n"

--Helper function that escapes characters in strings
escapeChars :: String -> String
escapeChars [] = []
escapeChars ('\\':xs) = '\\' : '\\' : escapeChars xs
escapeChars ('\"':xs) = '\\' : '\"' : escapeChars xs
escapeChars (x:xs) = x : escapeChars xs
