{-
    BNF Converter: C++ Skeleton generation
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

    Description   : This module generates the C++ Skeleton functions.

                    The generated files use the Visitor design pattern.

    Author        : Michael Pellauer (pellauer@cs.chalmers.se)

    License       : GPL (GNU General Public License)

    Created       : 9 August, 2003

    Modified      : 12 August, 2003


   **************************************************************
-}

module BNFC.Backend.CPP.NoSTL.CFtoCVisitSkel (cf2CVisitSkel) where

import BNFC.CF
import BNFC.Utils ((+++), (++++))
import BNFC.Backend.Common.NamedVariables
import Data.List
import Data.Char(toLower, toUpper)

--Produces (.H file, .C file)
cf2CVisitSkel :: CF -> (String, String)
cf2CVisitSkel cf = (mkHFile cf groups, mkCFile cf groups)
 where
    groups = fixCoercions (ruleGroups cf)


{- **** Header (.H) File Functions **** -}

--Generates the Header File
mkHFile :: CF -> [(Cat,[Rule])] -> String
mkHFile cf groups = unlines
 [
  header,
  concatMap prDataH groups,
  concatMap (prUserH.show) user,
  footer
 ]
 where
  user = fst (unzip (tokenPragmas cf))
  header = unlines
   [
    "#ifndef SKELETON_HEADER",
    "#define SKELETON_HEADER",
    "/* You might want to change the above name. */",
    "",
    "#include \"Absyn.H\"",
    "",
    "class Skeleton : public Visitor",
    "{",
    " public:"
   ]
  prUserH u = "  void visit" ++ u' ++ "(" ++ u ++ " p);"
   where
    u' = ((toUpper (head u)) : (map toLower (tail u))) --this is a hack to fix a potential capitalization problem.
  footer = unlines
   [
    "  void visitIdent(String s);",
    "  void visitInteger(Integer i);",
    "  void visitDouble(Double d);",
    "  void visitChar(Char c);",
    "  void visitString(String s);",
    "};",
    "",
    "#endif"
   ]

--Prints out visit functions for a category
prDataH :: (Cat, [Rule]) -> String
prDataH (cat, rules) =
 if "List" `isPrefixOf` (identCat cat)
 then concat ["  void visit", cl, "(", cl, "* ", vname, ");"]
 else abstract ++ (concatMap prRuleH rules)
 where
   cl = identCat (normCat cat)
   vname = map toLower cl
   abstract = case lookupRule (show cat) rules of
    Just x -> ""
    Nothing ->  "  void visit" ++ cl ++ "(" ++ cl ++ "*" +++ vname ++ "); /* abstract class */\n"

--Visit functions for a rule.
prRuleH :: Rule -> String
prRuleH (Rule fun c cats) | not (isCoercion fun) = concat
  ["  void visit", fun, "(", fun, "* ", fnm, ");\n"]
   where
    fnm = map toLower fun
prRuleH _ = ""


{- **** Implementation (.C) File Functions **** -}

--Makes the .C File
mkCFile :: CF -> [(Cat,[Rule])] -> String
mkCFile cf groups = concat
   [
    header,
    concatMap (prData user) groups,
    concatMap (prUser.show) user,
    footer
   ]
  where
    user = fst (unzip (tokenPragmas cf))
    header = unlines [
      "/*** BNFC-Generated Visitor Design Pattern Skeleton. ***/",
      "/* This implements the common visitor design pattern.",
      "   Note that this method uses Visitor-traversal of lists, so",
      "   List->accept() does NOT traverse the list. This allows different",
      "   algorithms to use context information differently. */",
      "",
      "#include \"Skeleton.H\"",
      ""
      ]
    prUser x = unlines
     [
      "void Skeleton::visit" ++ x' ++ "(" ++ x ++ " p)",
      "{",
      "  /* Code for " ++ x ++ " Goes Here */",
      "}"
     ]
     where
       x' = ((toUpper (head x)) : (map toLower (tail x))) --this is a hack to fix a potential capitalization problem.
    footer = unlines
     [
      "void Skeleton::visitIdent(Ident i)",
      "{",
      "  /* Code for Ident Goes Here */",
      "}",
      "void Skeleton::visitInteger(Integer i)",
      "{",
      "  /* Code for Integers Goes Here */",
      "}",
      "void Skeleton::visitDouble(Double d)",
      "{",
      "  /* Code for Doubles Goes Here */",
      "}",
      "void Skeleton::visitChar(Char c)",
      "{",
      "  /* Code for Chars Goes Here */",
      "}",
      "void Skeleton::visitString(String s)",
      "{",
      "  /* Code for Strings Goes Here */",
      "}",
      ""
     ]

--Visit functions for a category.
prData :: [UserDef] -> (Cat, [Rule]) -> String
prData user (cat, rules) =
 if "List" `isPrefixOf` (identCat cat)
 then unlines
 [
  "void Skeleton::visit" ++ cl ++ "("++ cl ++ "*" +++ vname ++ ")",
  "{",
  "  while(" ++ vname ++ "!= 0)",
  "  {",
  "    /* Code For " ++ cl ++ " Goes Here */",
  visitMember,
  "    " ++ vname ++ " = " ++ vname ++ "->" ++ vname ++ "_;",
  "  }",
  "}",
  ""
 ] --Not a list:
 else abstract ++ (concatMap (prRule user) rules)
 where
   cl = identCat (normCat cat)
   vname = map toLower cl
   ecl = identCat (normCatOfList cat)
   member = map toLower ecl ++ "_"
   visitMember = if isBasic user member
     then "    visit" ++ (funName member) ++ "(" ++ vname ++ "->" ++ member ++ ");"
     else "    " ++ vname ++ "->" ++ member ++ "->accept(this);"
   abstract = case lookupRule (show cat) rules of
    Just x -> ""
    Nothing ->  "void Skeleton::visit" ++ cl ++ "(" ++ cl ++ "*" +++ vname ++ ") {} //abstract class\n\n"

--Visits all the instance variables of a category.
prRule :: [UserDef] -> Rule -> String
prRule user (Rule fun c cats) | not (isCoercion fun) = unlines
  [
   "void Skeleton::visit" ++ fun ++ "(" ++ fun ++ "*" +++ fnm ++ ")",
   "{",
   "  /* Code For " ++ fun ++ " Goes Here */",
   "",
   cats' ++ "}\n"
  ]
   where
    cats' = if allTerms cats
        then ""
    	else (concatMap (prCat user fnm) (fixOnes (numVars [] cats)))
    allTerms [] = True
    allTerms ((Left z):zs) = False
    allTerms (z:zs) = allTerms zs
    fnm = map toLower fun
prRule user _ = ""

--Prints the actual instance-variable visiting.
prCat user fnm c =
  case c of
    (Right t) -> ""
    (Left nt) -> if isBasic user nt
       then "  visit" ++ (funName nt) ++ "(" ++ fnm ++ "->" ++ nt ++ ");\n"
       else if "list" `isPrefixOf` nt
         then "  if (" ++ fnm ++ "->" ++ nt ++ ") {" ++ accept ++ "}\n"
	 else "  " ++ accept ++ "\n"
      where
       accept = fnm ++ "->" ++ nt ++ "->accept(this);"

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
   user' = map (map toLower.show) user

--The visit-function name of a basic type
funName :: String -> String
funName v =
    if "integer_" `isPrefixOf` v then "Integer"
    else if "char_" `isPrefixOf` v then "Char"
    else if "string_" `isPrefixOf` v then "String"
    else if "double_" `isPrefixOf` v then "Double"
    else if "ident_" `isPrefixOf` v then "Ident"
    else (toUpper (head v)) : (init (tail v)) --User-defined type
