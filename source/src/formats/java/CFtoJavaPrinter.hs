{-
    BNF Converter: Java Pretty Printer generator
    Copyright (C) 2004  Author:  Michael Pellauer

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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

    Author        : Michael Pellauer (pellauer@cs.chalmers.se)

    License       : GPL (GNU General Public License)

    Created       : 24 April, 2003

    Modified      : 2 September, 2003

    Added string buffer for efficiency (Michael, August 03)
   **************************************************************
-}
module CFtoJavaPrinter ( cf2JavaPrinter ) where

import CF
import NamedVariables
import Utils		( (+++) )
import Data.List
import Data.Char		( toLower )

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
    unlines (map (prData packageAbsyn) groups),
    unlines (map (shData packageAbsyn) groups),
    footer
   ]
  where
    groups = (fixCoercions (ruleGroupsInternals cf))
    header = unlines [
      "package" +++ packageBase ++ ";",
      "import" +++ packageAbsyn ++ ".*;",
      "",
      "public class PrettyPrinter",
      "{",
      "  //For certain applications increasing the initial size of the buffer may improve performance.",
      "  private static final int INITIAL_BUFFER_SIZE = 128;",
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
      "  private static void sh(Integer n) { buf_.append(n); }",
      "  private static void sh(Double d) { buf_.append(d); }",
      "  private static void sh(Character c) { buf_.append(c); }",
      "  private static void sh(String s) { printQuoted(s); }",
      "  private static void printQuoted(String s) { buf_.append(\"\\\"\" + s + \"\\\"\"); }",
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
      "   String s = buf_.toString();", -- peteg: java 1.1.8 compatibility.
      "",
      "     if (s.substring(buf_.length() - 1, buf_.length()).equals(\" \")) {",
      "       buf_.setCharAt(buf_.length() - 1, '\\\"');",
      "       buf_.setLength(buf_.length() - 1);",
      "     }",
      "  }",
      "  private static int _n_ = 0;",
      "  private static StringBuffer buf_ = new StringBuffer(INITIAL_BUFFER_SIZE);",
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
      "       _n_ = _n_ + 2;",
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
      "       _n_ = _n_ - 2;",
      "       backup();",
      "       backup();",
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
    msg ++ concat (map prEntryPoint (allEntryPoints cf)) ++ msg2
 where
  msg = "  //  print and show methods are defined for each Entry Point type.\n"
  msg2 = "  /***   You shouldn't need to change anything beyond this point.   ***/\n"
  prEntryPoint cat | (normCat cat) == cat = unlines
   [
    "  public static String print(" ++ packageAbsyn ++ "." ++ cat' ++ " foo)",
    "  {",
    "    pp(foo, 0);",
    "    String temp = buf_.toString();",
    "    buf_ = new StringBuffer(INITIAL_BUFFER_SIZE);",
    "    return temp;",
    "  }",
    "  public static String show(" ++ packageAbsyn ++ "." ++ cat' ++ " foo)",
    "  {",
    "    sh(foo);",
    "    String temp = buf_.toString();",
    "    buf_ = new StringBuffer(INITIAL_BUFFER_SIZE);",
    "    return temp;",
    "  }"
   ]
   where
    cat' = identCat cat
  prEntryPoint _ = ""

prData :: String -> (Cat, [Rule]) -> String
prData packageAbsyn (cat, rules) =
 if isList cat
 then unlines
 [
  "  private static void pp(" ++ packageAbsyn ++ "."
      ++ identCat (normCat cat) +++ "foo, int _i_)",
  "  {",
  (prList cat rules) ++ "  }"
 ]
 else unlines --not a list
 [
  "  private static void pp(" ++ packageAbsyn ++ "." ++ identCat (normCat cat) +++ "foo, int _i_)",
  "  {",
  (concat (map (prRule packageAbsyn) rules)) ++ "  }"
 ]

prRule :: String -> Rule -> String
prRule packageAbsyn r@(fun, (_c, cats))
    | not (isCoercion fun) && not (isDefinedRule fun) = concat
  [
   "    if (foo instanceof" +++ packageAbsyn ++ "." ++ fun ++ ")\n",
   "    {\n",
   "       " ++ packageAbsyn ++ "." ++ fun +++ fnm +++ "= ("
     ++ packageAbsyn ++ "." ++ fun ++ ") foo;\n",
   lparen,
   cats',
   rparen,
   "    }\n"
  ]
   where
    p = precRule r
    (lparen, rparen) =
     ("       if (_i_ > " ++ (show p) ++ ") render(_L_PAREN);\n",
      "       if (_i_ > " ++ (show p) ++ ") render(_R_PAREN);\n")
    cats' = case cats of
        [] -> ""
    	_  -> concatMap (prCat fnm) (zip (fixOnes (numVars [] cats)) (map getPrec cats))
    fnm = '_' : map toLower fun

    getPrec (Right {}) = 0
    getPrec (Left  c)  = precCat c

prRule _nm (_fun, _cats) = ""

prList :: Cat -> [Rule] -> String
prList c rules = unlines
  [
   "    while (foo != null)",
   "    {",
   "      if (foo." ++ c'' ++ "_ == null)",
   "      {",
   "        pp(foo." ++ c' ++ "_, 0);",
   optsep,
   "      }",
   "      else",
   "      {",
   "        pp(foo." ++ c' ++ "_, 0);",
   "        render(\"" ++ (escapeChars sep) ++ "\");",
   "      }",
   "      foo = foo." ++ c'' ++ "_;",
   "    }"
  ]
 where
    c' = map toLower (identCat (normCatOfList c))
    c'' = map toLower (identCat c)
    sep = getCons rules
    optsep = if hasOneFunc rules then "" else ("        render(\"" ++ (escapeChars sep) ++ "\");")

getCons :: [Rule] -> String
getCons ((f, (_c, cats)):rs) =
 if isConsFun f
   then seper cats
   else getCons rs
 where
    seper [] = []
    seper ((Right x):_xs) = x
    seper ((Left {}):xs) = seper xs

hasOneFunc :: [Rule] -> Bool
hasOneFunc [] = False
hasOneFunc ((f, (_, _cats)):rs) =
 if (isOneFun f)
    then True
    else hasOneFunc rs

prCat fnm (c, p) =
    case c of
	   Right t -> "       render(\"" ++ escapeChars t ++ "\");\n"
	   Left nt | "string" `isPrefixOf` nt
                     -> "       printQuoted(" ++ fnm ++ "." ++ nt ++ ");\n"
                   | isInternalVar nt -> ""
                   | otherwise
                     -> "       pp(" ++ fnm ++ "." ++ nt ++ ", " ++ show p ++ ");\n"

--The following methods generate the Show function.

shData :: String -> (Cat, [Rule]) -> String
shData packageAbsyn (cat, rules) =
 if isList cat
 then unlines
 [
  "  private static void sh(" ++ packageAbsyn ++ "." ++ identCat (normCat cat) +++ "foo)",
  "  {",
  (shList cat rules) ++ "  }"
 ]
 else unlines
 [
  "  private static void sh(" ++ packageAbsyn ++ "." ++ identCat (normCat cat) +++ "foo)",
  "  {",
  (concat (map (shRule packageAbsyn) rules)) ++ "  }"
 ]

shRule :: String -> Rule -> String
shRule packageAbsyn (fun, (_c, cats))
    | not (isCoercion fun) && not (isDefinedRule fun) = unlines
  [
   "    if (foo instanceof" +++ packageAbsyn ++ "." ++ fun ++ ")",
   "    {",
   "       " ++ packageAbsyn ++ "." ++ fun +++ fnm +++ "= ("
     ++ packageAbsyn ++ "." ++ fun ++ ") foo;",
   members ++ "    }"
  ]
   where
    members = concat
     [
      lparen,
      "       render(\"" ++ (escapeChars fun) ++ "\");\n",
      cats',
      rparen
     ]
    cats' = if allTerms cats
        then ""
    	else (concat (map (shCat fnm) (fixOnes (numVars [] cats))))
    (lparen, rparen) =
      if allTerms cats
         then ("","")
 	 else ("       render(\"(\");\n","       render(\")\");\n")
    allTerms [] = True
    allTerms ((Left {}):_) = False
    allTerms (_:zs) = allTerms zs
    fnm = '_' : map toLower fun
shRule _nm (_fun, _cats) = ""

shList :: Cat -> [Rule] -> String
shList c _rules = unlines
  [
   "    while (foo != null)",
   "    {",
   "      if (foo." ++ c'' ++ "_ == null)",
   "      {",
   "        sh(foo." ++ c' ++ "_);",
   "      }",
   "      else",
   "      {",
   "        sh(foo." ++ c' ++ "_);",
   "        render(\",\");",
   "      }",
   "      foo = foo." ++ c'' ++ "_;",
   "    }"
  ]
 where
    c' = map toLower (identCat (normCatOfList c))
    c'' = map toLower (identCat c)


shCat fnm c =
    case c of
    Right {} -> ""
    Left nt | "list" `isPrefixOf` nt
                -> unlines ["       render(\"[\");",
		            "       sh(" ++ fnm ++ "." ++ nt ++ ");",
		            "       render(\"]\");"]
            | isInternalVar nt -> ""
            | otherwise  -> "       sh(" ++ fnm ++ "." ++ nt ++ ");\n"


--Helper function that escapes characters in strings
escapeChars :: String -> String
escapeChars [] = []
escapeChars ('\\':xs) = '\\' : ('\\' : (escapeChars xs))
escapeChars ('\"':xs) = '\\' : ('\"' : (escapeChars xs))
escapeChars (x:xs) = x : (escapeChars xs)

isInternalVar x = x == internalCat ++ "_"
