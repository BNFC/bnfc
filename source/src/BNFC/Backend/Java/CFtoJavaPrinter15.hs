{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-
    BNF Converter: Java Pretty Printer generator
    Copyright (C) 2004  Author:  Michael Pellauer, Bjorn Bringert

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

    Created       : 24 April, 2003

    Modified      : 9 Aug, 2004
    Added string buffer for efficiency (Michael, August 03)

-}

module BNFC.Backend.Java.CFtoJavaPrinter15 ( cf2JavaPrinter ) where

import Prelude hiding ((<>))

import Data.Bifunctor ( second )
import Data.Char      ( toLower, isSpace )
import Data.Either    ( lefts )
import Data.List      ( intersperse )
import Data.Maybe     ( isJust )

import BNFC.CF
import BNFC.PrettyPrint
import BNFC.Utils     ( (+++), for, unless, unlessNull, uniqOn )

import BNFC.Backend.Common ( switchByPrecedence )
import BNFC.Backend.Common.NamedVariables
import BNFC.Backend.Java.CFtoJavaAbs15

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
      "  private static void pp(Double d, int _i_) { buf_.append(String.format(java.util.Locale.ROOT, \"%.15g \", d)); }",
      "  private static void pp(String s, int _i_) { buf_.append(s); buf_.append(\" \"); }",
      "  private static void pp(Character c, int _i_) { buf_.append(\"'\" + c.toString() + \"'\"); buf_.append(\" \"); }",
      "  private static void sh(Integer n) { render(n.toString()); }",
      "  private static void sh(Double d) { render(String.format(java.util.Locale.ROOT, \"%.15g\", d)); }",
      "  private static void sh(Character c) { render(\"'\" + c.toString() + \"'\"); }",
      "  private static void sh(String s) { printQuoted(s); }",
      "",
      "  private static void printQuoted(String s) { render(\"\\\"\" + s + \"\\\"\"); }",
      "",
      "  private static void indent()",
      "  {",
      "    int n = _n_;",
      "    while (n > 0)",
      "    {",
      "      buf_.append(\' \');",
      "      n--;",
      "    }",
      "  }",
      "",
      "  private static void backup()",
      "  {",
      "    int prev = buf_.length() - 1;",
      "    if (prev >= 0 && buf_.charAt(prev) == ' ')",
      "      buf_.setLength(prev);",
      "  }",
      "",
      "  private static void trim()",
      "  {",
      "    // Trim initial spaces",
      "    int end = 0;",
      "    int len = buf_.length();",
      "    while (end < len && buf_.charAt(end) == ' ')",
      "      end++; ",
      "    buf_.delete(0, end);",
      "    // Trim trailing spaces",
      "    removeTrailingSpaces();",
      "  }",
      "",
      "  private static void removeTrailingSpaces()",
      "  {",
      "    int end = buf_.length();",
      "    while (end > 0 && buf_.charAt(end-1) == ' ')",
      "      end--;",
      "    buf_.setLength(end);",
      "  }",
      "",
      "  private static void onEmptyLine()",
      "  {",
      "    removeTrailingSpaces();",
      "    int len = buf_.length();",
      "    if (len > 0 && buf_.charAt(len-1) != '\\n') buf_.append(\"\\n\");",
      "    indent();",
      "  }",
      "",
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
      "       onEmptyLine();",
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
      "       _n_ = _n_ - INDENT_WIDTH;",
      "       onEmptyLine();",
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
      "    else if (s.trim().equals(\"\"))",
      "    {",
      "       backup();",
      "       buf_.append(s);",
      "    }",
      "    else",
      "    {",
      "       buf_.append(s);",
      "       buf_.append(\" \");",
      "    }",
      "  }"
  ]

prEntryPoints :: String -> CF -> String
prEntryPoints packageAbsyn cf =
    msg ++ concatMap prEntryPoint (allCatsNorm cf) ++ msg2
 where
  msg = "  //  print and show methods are defined for each category.\n"
  msg2 = "  /***   You shouldn't need to change anything beyond this point.   ***/\n"
  prEntryPoint cat = unlines
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

prData :: String ->  [UserDef] -> (Cat, [Rule]) -> String
prData packageAbsyn user (cat, rules)
  | isList cat = unlines $ concat
      [ [ "  private static void pp(" ++ packageAbsyn ++ "." ++ dat ++ " foo, int _i_)"
        , "  {"
        , "    pp" ++ dat ++ "(foo.iterator(), _i_);"
        , "  }"
        , ""
        , "  private static void pp" ++ dat ++ "(java.util.Iterator<" ++ et ++ "> it, int _i_)"
        , "  {"
        ]
      , map ("    " ++) $ prList dat et rules
      , [ "  }"
        , ""
        ]
      ]
  | otherwise = unlines $
      [ "  private static void pp(" ++ packageAbsyn ++ "."
           ++ dat +++ "foo, int _i_)"
      , "  {"
      , concat (addElse $ map (prRule packageAbsyn) rules)
      , "  }"
      ]
  where
  dat = identCat (normCat cat)
  et  = typename packageAbsyn user $ identCat $ normCatOfList cat
  addElse = map ("    " ++) . intersperse "else " . filter (not . null)
    . map (dropWhile isSpace)


prRule :: String -> Rule -> String
prRule packageAbsyn r@(Rule f _c cats _) | not (isCoercion f || isDefinedRule f) = concat
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
    fun = funName f
    p = precRule r
    (lparen, rparen) =
        ("       if (_i_ > " ++ show p ++ ") render(_L_PAREN);\n",
        "       if (_i_ > " ++ show p ++ ") render(_R_PAREN);\n")
    cats' = case cats of
        [] -> ""
        _  -> concatMap (render . prItem (text fnm)) (numVars cats)
    fnm = '_' : map toLower fun

prRule _nm _ = ""

prList :: String -> String -> [Rule] -> [String]
prList dat et rules = concat
    [ if null docs0 then
      [ "if (it.hasNext())" ]
      else
      [ "if (!it.hasNext())"
      , "{ /* nil */"
      , render $ nest 4 $ vcat docs0
      , "}"
      , "else"
      ]
    , if null docs1 then
      [ "{ /* cons */"
      , "  " ++ et ++ " el = it.next();"
      ]
      else
      [ "{"
      , "  " ++ et ++ " el = it.next();"
      , "  if (!it.hasNext())"
      , "  { /* last */"
      , render $ nest 4 $ vcat docs1
      , "  }"
      , "  else"
      , "  { /* cons */"
      ]
    , unlessNull (swRules isConsFun) $ \ docs ->
      [ render $ nest (if null docs1 then 2 else 4) $ vcat docs
      ]
    , unless (null docs1) [ "  }" ]
    , [ "}" ]
    ]
  where
  prules      = sortRulesByPrecedence rules
  swRules f   = switchByPrecedence "_i_" $
                  map (second $ sep . map text . prListRule_ dat) $
                    uniqOn fst $ filter f prules
                    -- Discard duplicates, can only handle one rule per precedence.
  docs0       = swRules isNilFun
  docs1       = swRules isOneFun

-- | Only render the rhs (items) of a list rule.

prListRule_ :: IsFun a => String -> Rul a -> [String]
prListRule_ dat (Rule _ _ items _) = for items $ \case
  Right t                  -> "render(\"" ++ escapeChars t ++ "\");"
  Left (TokenCat "String") -> "printQuoted(el);"
  Left (ListCat _)         -> "pp" ++ dat ++ "(it, _i_);"
  Left _                   -> "pp(el, _i_);"

-- |
-- >>> prItem "F" (Right "++")
--        render("++");
-- <BLANKLINE>
-- >>> prItem "F" (Left (TokenCat "String", "string_"))
--        printQuoted(F.string_);
-- <BLANKLINE>
-- >>> prItem "F" (Left (Cat "Abc", "abc_"))
--        pp(F.abc_, 0);
-- <BLANKLINE>
prItem :: Doc -> Either (Cat, Doc) String -> Doc
prItem _ (Right t) = nest 7 ("render(\"" <> text(escapeChars t) <> "\");\n")
prItem fnm (Left (TokenCat "String", nt))
    = nest 7 ("printQuoted(" <> fnm <> "." <> nt <> ");\n")
prItem fnm (Left (cat, nt))
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
          , shList packageAbsyn user cat rules ++ "  }"
          ]
          else
          [ "  private static void sh(" ++ packageAbsyn ++ "."
                ++ identCat (normCat cat) +++ "foo)"
          , "  {"
          , concatMap (shRule packageAbsyn) rules ++ "  }"
          ]


shRule :: String -> Rule -> String
shRule packageAbsyn (Rule f _c cats _) | not (isCoercion f || isDefinedRule f) = unlines
    [ "    if (foo instanceof" +++ packageAbsyn ++ "." ++ fun ++ ")"
    , "    {"
    , "       " ++ packageAbsyn ++ "." ++ fun +++ fnm +++ "= ("
        ++ packageAbsyn ++ "." ++ fun ++ ") foo;"
    , members ++ "    }"
    ]
  where
    fun = funName f
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

shList :: String -> [UserDef] -> Cat -> [Rule] -> String
shList packageAbsyn user c _rules = unlines
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
    et = typename packageAbsyn user $ identCat $ normCatOfList c

-- |
-- >>> shCat "F" (ListCat (Cat "A"), "lista_")
--        render("[");
--        sh(F.lista_);
--        render("]");
-- <BLANKLINE>
-- >>> shCat "F" (Cat "A", "a_")
--        sh(F.a_);
-- <BLANKLINE>
shCat :: Doc -> (Cat, Doc) -> Doc
shCat fnm (ListCat _, vname) = vcat
    [ "       render(\"[\");"
    , "       sh(" <> fnm <> "." <> vname <> ");"
    , "       render(\"]\");\n" ]
shCat fname (_, vname)       = "       sh(" <> fname <> "." <> vname <> ");\n"

--Helper function that escapes characters in strings
escapeChars :: String -> String
escapeChars [] = []
escapeChars ('\\':xs) = '\\' : '\\' : escapeChars xs
escapeChars ('\"':xs) = '\\' : '\"' : escapeChars xs
escapeChars (x:xs) = x : escapeChars xs
