{-
    BNF Converter: C# Pretty Printer Generator
    Copyright (C) 2006  Author:  Johan Broberg

    Modified from CFtoSTLPrinter

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

    Description   : This module generates the C# Pretty Printer.
                    It also generates the "show" method for
                    printing an abstract syntax tree.

    Author        : Johan Broberg (johan@pontemonti.com)

    License       : GPL (GNU General Public License)

    Created       : 26 November, 2006

    Modified      : 21 January, 2007 by Johan Broberg

   **************************************************************
-}

module BNFC.Backend.CSharp.CFtoCSharpPrinter (cf2csharpprinter) where

import BNFC.CF
import BNFC.Utils ((+++))
import BNFC.Backend.Common.NamedVariables
import Data.List
import Data.Char(toLower)
import Data.Maybe
import BNFC.Backend.CSharp.CSharpUtils

--Produces .cs file
cf2csharpprinter :: Namespace -> CF -> String
cf2csharpprinter namespace cf = unlinesInline [
  header namespace cf,
  "    ",
  entrypoints namespace cf,
  "    ",
  "    #region (Internal) Print Methods",
  unlinesInlineMap (prData namespace user) groups,
  "    #endregion",
  "    ",
  "    #region (Internal) Show Methods",
  unlinesInlineMap (shData namespace user) groups,
  "    #endregion",
  "  }",
  "  #endregion",
  "}"
  ]
  where
    groups = fixCoercions (ruleGroupsInternals cf)
    user = [n | (n,_) <- tokenPragmas cf]

header :: Namespace -> CF -> String
header namespace cf = unlinesInline [
  "/*** BNFC-Generated Pretty Printer and Abstract Syntax Viewer ***/",
  " ",
  -- imports
  "using System;",
  "using System.Text; // for StringBuilder",
  "using " ++ namespace ++ ".Absyn;",
  " ",
  "namespace " ++ namespace,
  "{",
  "  #region Pretty-printer class",
  "  public class PrettyPrinter",
  "  {",
  "    #region Misc rendering functions",
  "    // You may wish to change these:",
  "    private const int BUFFER_INITIAL_CAPACITY = 2000;",
  "    private const int INDENT_WIDTH = 2;",
  "    private const string LEFT_PARENTHESIS = \"(\";",
  "    private const string RIGHT_PARENTHESIS = \")\";",
  "    private static System.Globalization.NumberFormatInfo InvariantFormatInfo = System.Globalization.NumberFormatInfo.InvariantInfo;",
  "    ",
  "    private static int _n_ = 0;",
  "    private static StringBuilder buffer = new StringBuilder(BUFFER_INITIAL_CAPACITY);",
  "    ",
  "    //You may wish to change render",
  "    private static void Render(String s)",
  "    {",
  "      if(s == \"{\")",
  "      {",
  "        buffer.Append(\"\\n\");",
  "        Indent();",
  "        buffer.Append(s);",
  "        _n_ = _n_ + INDENT_WIDTH;",
  "        buffer.Append(\"\\n\");",
  "        Indent();",
  "      }",
  "      else if(s == \"(\" || s == \"[\")",
  "        buffer.Append(s);",
  "      else if(s == \")\" || s == \"]\")",
  "      {",
  "        Backup();",
  "        buffer.Append(s);",
  "        buffer.Append(\" \");",
  "      }",
  "      else if(s == \"}\")",
  "      {",
  "        int t;",
  "        _n_ = _n_ - INDENT_WIDTH;",
  "        for(t=0; t<INDENT_WIDTH; t++) {",
  "          Backup();",
  "        }",
  "        buffer.Append(s);",
  "        buffer.Append(\"\\n\");",
  "        Indent();",
  "      }",
  "      else if(s == \",\")",
  "      {",
  "        Backup();",
  "        buffer.Append(s);",
  "        buffer.Append(\" \");",
  "      }",
  "      else if(s == \";\")",
  "      {",
  "        Backup();",
  "        buffer.Append(s);",
  "        buffer.Append(\"\\n\");",
  "        Indent();",
  "      }",
  "      else if(s == \"\") return;",
  "      else",
  "      {",
  "        // Make sure escaped characters are printed properly!",
  "        if(s.StartsWith(\"\\\"\") && s.EndsWith(\"\\\"\"))",
  "        {",
  "          buffer.Append('\"');",
  "          StringBuilder sb = new StringBuilder(s);",
  "          // Remove enclosing citation marks",
  "          sb.Remove(0,1);",
  "          sb.Remove(sb.Length-1,1);",
  "          // Note: we have to replace backslashes first! (otherwise it will \"double-escape\" the other escapes)",
  "          sb.Replace(\"\\\\\", \"\\\\\\\\\");",
  "          sb.Replace(\"\\n\", \"\\\\n\");",
  "          sb.Replace(\"\\t\", \"\\\\t\");",
  "          sb.Replace(\"\\\"\", \"\\\\\\\"\");",
  "          buffer.Append(sb.ToString());",
  "          buffer.Append('\"');",
  "        }",
  "        else",
  "        {",
  "          buffer.Append(s);",
  "        }",
  "        buffer.Append(\" \");",
  "      }",
  "    }",
  "    ",
  "    private static void PrintInternal(int n, int _i_)",
  "    {",
  "      buffer.Append(n.ToString(InvariantFormatInfo));",
  "      buffer.Append(' ');",
  "    }",
  "    ",
  "    private static void PrintInternal(double d, int _i_)",
  "    {",
  "      buffer.Append(d.ToString(InvariantFormatInfo));",
  "      buffer.Append(' ');",
  "    }",
  "    ",
  "    private static void PrintInternal(string s, int _i_)",
  "    {",
  "      Render(s);",
  "    }",
  "    ",
  "    private static void PrintInternal(char c, int _i_)",
  "    {",
  "      PrintQuoted(c);",
  "    }",
  "    ",
  unlinesInlineMap (prToken namespace) (tokenNames cf),
  "    ",
  "    private static void ShowInternal(int n)",
  "    {",
  "      Render(n.ToString(InvariantFormatInfo));",
  "    }",
  "    ",
  "    private static void ShowInternal(double d)",
  "    {",
  "      Render(d.ToString(InvariantFormatInfo));",
  "    }",
  "    ",
  "    private static void ShowInternal(char c)",
  "    {",
  "      PrintQuoted(c);",
  "    }",
  "    ",
  "    private static void ShowInternal(string s)",
  "    {",
  "      PrintQuoted(s);",
  "    }",
  "    ",
  unlinesInlineMap (shToken namespace) (tokenNames cf),
  "    ",
  "    private static void PrintQuoted(string s)",
  "    {",
  "      Render(\"\\\"\" + s + \"\\\"\");",
  "    }",
  "    ",
  "    private static void PrintQuoted(char c)",
  "    {",
  "      // Makes sure the character is escaped properly before printing it.",
  "      string str = c.ToString();",
  "      if(c == '\\n') str = \"\\\\n\";",
  "      if(c == '\\t') str = \"\\\\t\";",
  "      Render(\"'\" + str + \"'\");",
  "    }",
  "    ",
  "    private static void Indent()",
  "    {",
  "      int n = _n_;",
  "      while (n > 0)",
  "      {",
  "        buffer.Append(' ');",
  "        n--;",
  "      }",
  "    }",
  "    ",
  "    private static void Backup()",
  "    {",
  "      if(buffer[buffer.Length - 1] == ' ')",
  "      {",
  "        buffer.Length = buffer.Length - 1;",
  "      }",
  "    }",
  "    ",
  "    private static void Trim()",
  "    {",
  "      while(buffer.Length > 0 && buffer[0] == ' ')",
  "        buffer.Remove(0, 1); ",
  "      while(buffer.Length > 0 && buffer[buffer.Length-1] == ' ')",
  "        buffer.Remove(buffer.Length-1, 1);",
  "    }",
  "    ",
  "    private static string GetAndReset()",
  "    {",
  "      Trim();",
  "      string strReturn = buffer.ToString();",
  "      Reset();",
  "      return strReturn;",
  "    }",
  "    ",
  "    private static void Reset()",
  "    {",
  "      buffer.Remove(0, buffer.Length);",
  "    }",
  "    #endregion"
  ]

prToken :: Namespace -> String -> String
prToken namespace token = unlinesInline [
  "    private static void PrintInternal(" ++ identifier namespace token ++ " token, int _i_)",
  "    {",
  "      buffer.Append('\\\"');",
  "      buffer.Append(token.ToString());",
  "      buffer.Append('\\\"');",
  "    }"
  ]

shToken :: Namespace -> String -> String
shToken namespace token = unlinesInline [
  "    private static void ShowInternal(" ++ identifier namespace token ++ " token)",
  "    {",
  "      Render(token.ToString());",
  "    }"
  ]

entrypoints :: Namespace -> CF -> String
entrypoints namespace cf = unlinesInline [
  "    #region Print Entry Points",
  unlinesInlineMap prEntryPoint (allCats cf),
  "    #endregion",
  "    ",
  "    #region Show Entry Points",
  unlinesInlineMap shEntryPoint (allCats cf),
  "    #endregion"
  ]
  where
    prEntryPoint cat | (normCat cat) == cat = unlinesInline [
      "    public static string Print(" ++ identifier namespace (identCat cat) ++ " cat)",
      "    {",
      "      PrintInternal(cat, 0);",
      "      return GetAndReset();",
      "    }"
      ]
    prEntryPoint _ = ""
    shEntryPoint cat | (normCat cat) == cat = unlinesInline [
      "    public static String Show(" ++ identifier namespace (identCat cat) ++ " cat)",
      "    {",
      "      ShowInternal(cat);",
      "      return GetAndReset();",
      "    }"
      ]
    shEntryPoint _ = ""

prData :: Namespace ->  [UserDef] -> (Cat, [Rule]) -> String
prData namespace user (cat, rules)
  -- list
  | isList cat = unlinesInline [
    "    private static void PrintInternal(" ++ identifier namespace (identCat (normCat cat)) ++ " p, int _i_)",
    "    {",
    (prList user cat rules),
    "    }"
    ]
  -- not a list
  | otherwise = unlinesInline [
    "    private static void PrintInternal(" ++ identifier namespace (identCat (normCat cat)) ++ " p, int _i_)",
    "    {",
    -- first rule starts with "if", the rest of them start with "else if".
    -- this isn't very pretty, but does the job and produces nice code.
    prRule namespace Nothing firstRule,
    unlinesInline $ map (prRule namespace (Just "else ")) otherRules,
    "    }"
    ]
    where
      -- Removes the rules at the beginning of the list which won't be used by the prRule function.
      rules' = dropWhile (\r -> isCoercion (funRule r) || isDefinedRule (funRule r)) rules
      firstRule = head rules'
      otherRules = tail rules'

prRule :: Namespace -> Maybe String -> Rule -> String
prRule namespace maybeElse r@(Rule fun _c cats)
  | not (isCoercion fun || isDefinedRule fun) = unlinesInline [
    "      " ++ fromMaybe "" maybeElse ++ "if(p is " ++ identifier namespace fun ++ ")",
    "      {",
    "        " ++ identifier namespace fun +++ fnm ++ " = (" ++ identifier namespace fun ++ ")p;",
    "        if(_i_ > " ++ (show p) ++ ") Render(LEFT_PARENTHESIS);",
    cats',
    "        if(_i_ > " ++ (show p) ++ ") Render(RIGHT_PARENTHESIS);",
    "      }"
    ]
    where
      p = precRule r
      cats' = case cats of
        [] -> ""
        _  -> unlinesInline $ map (prCat fnm) (zip (fixOnes (numProps [] cats)) (map getPrec cats))
      fnm = '_' : map toLower fun

      getPrec (Right {}) = 0
      getPrec (Left  c)  = precCat c
prRule _nm _ _ = ""

prList :: [UserDef] -> Cat -> [Rule] -> String
prList _ _ rules = unlinesInline [
  "      for(int i=0; i < p.Count; i++)",
  "      {",
  "        PrintInternal(p[i], 0);",
  "        if(i < p.Count - 1)",
  "        {",
  "          Render(\"" ++ escapeChars sep ++ "\");",
  "        }",
  "        else",
  "        {",
  "          Render(\"" ++ optsep ++ "\");",
  "        }",
  "      }"
  ]
  where
    sep = getCons rules
    optsep = if hasOneFunc rules then "" else escapeChars sep

prCat fnm (c, p) =
  case c of
    Right t -> "        Render(\"" ++ escapeChars t ++ "\");"
    Left nt
      | "string" `isPrefixOf` nt -> "        PrintQuoted(" ++ fnm ++ "." ++ nt ++ ");"
      | isInternalVar nt         -> ""
      | otherwise                -> "        PrintInternal(" ++ fnm ++ "." ++ nt ++ ", " ++ show p ++ ");"


--The following methods generate the Show function.

shData :: Namespace -> [UserDef] -> (Cat, [Rule]) -> String
shData namespace user (cat, rules)
  | isList cat = unlinesInline [
    "    private static void ShowInternal(" ++ identifier namespace (identCat (normCat cat)) ++ " p)",
    "    {",
    (shList user cat rules),
    "    }"
    ]
  | otherwise = unlinesInline [
    "    private static void ShowInternal(" ++ identifier namespace (identCat (normCat cat)) ++ " p)",
    "    {",
    unlinesInline $ map (shRule namespace) rules,
    "    }"
    ]

shRule :: Namespace -> Rule -> String
shRule namespace (Rule fun _c cats)
  | not (isCoercion fun || isDefinedRule fun) = unlinesInline [
    "      if(p is " ++ identifier namespace fun ++ ")",
    "      {",
    "        " ++ identifier namespace fun +++ fnm ++ " = (" ++ identifier namespace fun ++ ")p;",
    lparen,
    "        Render(\"" ++ (escapeChars fun) ++ "\");",
    cats',
    rparen,
    "      }"
    ]
  where
    cats' | allTerms cats = ""
          | otherwise     = unlinesInline $ map (shCat fnm) (fixOnes (numProps [] cats))
    lparen | allTerms cats = ""
           | otherwise     = "        Render(\"(\");"
    rparen | allTerms cats = ""
           | otherwise     = "        Render(\")\");"
    allTerms [] = True
    allTerms ((Left {}):_) = False
    allTerms (_:zs) = allTerms zs
    fnm = '_' : map toLower fun
shRule _nm _ = ""

shList :: [UserDef] -> Cat -> [Rule] -> String
shList _ _ _rules = unlinesInline [
  "      for(int i=0; i < p.Count; i++)",
  "      {",
  "        ShowInternal(p[i]);",
  "        if(i < p.Count - 1)",
  "          Render(\",\");",
  "      }"
  ]

shCat fnm c =
  case c of
    Right {} -> ""
    Left nt
      | "list" `isPrefixOf` nt -> unlinesInline [
          "        Render(\"[\");",
          "        ShowInternal(" ++ fnm ++ "." ++ nt ++ ");",
          "        Render(\"]\");"
          ]
      | isInternalVar nt       -> ""
      | otherwise              -> "        ShowInternal(" ++ fnm ++ "." ++ nt ++ ");"

isInternalVar x = x == show InternalCat ++ "_"
