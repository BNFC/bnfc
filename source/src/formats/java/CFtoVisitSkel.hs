{-
    BNF Converter: Java Vistor skeleton generator
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

    Description   : This module generates a Skeleton function
                    which uses the Visitor Design Pattern, which
                    users may find more familiar than Appel's
                    method.

    Author        : Michael Pellauer (pellauer@cs.chalmers.se)

    License       : GPL (GNU General Public License)

    Created       : 4 August, 2003                           

    Modified      : 2 September, 2003                          

   
   ************************************************************** 
-}
module CFtoVisitSkel (cf2VisitSkel) where

import CF
import Utils ((+++), (++++))
import NamedVariables
import Data.List
import Data.Char(toLower, toUpper)

--Produces a Skeleton using the Visitor Design Pattern.
--Thus the user can choose which Skeleton to use.

cf2VisitSkel :: String -> String -> CF -> String
cf2VisitSkel packageBase packageAbsyn cf = 
  concat [
    header,
    concatMap (prData packageAbsyn user) groups,
    concatMap prUser user,
    footer]
  where
    user = fst (unzip (tokenPragmas cf))
    groups = (fixCoercions (ruleGroups cf))
    header = unlines [
      "package" +++ packageBase ++ ";",
      "import" +++ packageAbsyn ++ ".*;",
      "/*** BNFC-Generated Visitor Design Pattern Skeleton. ***/",
      "/* This implements the common visitor design pattern.",
      "   Tests show it to be slightly less efficient than the",
      "   instanceof method, but easier to use.",
      "   Note that this method uses Visitor-traversal of lists, so",
      "   List.accept() does NOT traverse the list. This allows different",
      "   algorithms to use context information differently. */",
      "",
      "public class VisitSkel implements Visitor",
      "{"
      ]
    prUser u = "  public void visit" ++ u' ++ "(String p) {}\n"
      where
       u' = ((toUpper (head u)) : (map toLower (tail u))) --this is a hack to fix a potential capitalization problem.
    footer = unlines 
     [ --later only include used categories
      "  public void visitIdent(String i) {}",
      "  public void visitInteger(Integer i) {}",
      "  public void visitDouble(Double d) {}",
      "  public void visitChar(Character c) {}",
      "  public void visitString(String s) {}",
      "}"
     ]
     
--Traverses a category based on its type.
prData :: String -> [UserDef] -> (Cat, [Rule]) -> String
prData packageAbsyn user (cat, rules) = 
 if isList cat
 then unlines
 [
  "  public void visit" ++ cl ++ "(" ++ packageAbsyn ++ "." ++ cl +++ vname ++ ")",
  "  {",
  "    while(" ++ vname ++ "!= null)",
  "    {",
  "      /* Code For" +++ cl +++ "Goes Here */",
  visitMember,
  "      " ++ vname +++ "=" +++ vname ++ "." ++ vname ++ "_;",
  "    }",
  "  }"
 ] --Not a list:
 else abstract ++ (concatMap (prRule packageAbsyn user) rules)
 where
   cl = identCat (normCat cat)
   ecl = identCat (normCatOfList cat)
   vname = map toLower cl
   member = map toLower ecl ++ "_"
   visitMember = if isBasic user member
     then "    visit" ++ (funName member) ++ "(" ++ vname ++ "." ++ member ++ ");"
     else "    " ++ vname ++ "." ++ member ++ ".accept(this);"
   abstract = case lookupRule cat rules of
    Just x -> ""
    Nothing ->  "  public void visit" ++ cl ++ "(" ++ packageAbsyn ++ "."
		  ++ cl +++ vname ++ ") {} //abstract class\n"

--traverses a standard rule.
prRule :: String -> [UserDef] -> Rule -> String
prRule packageAbsyn user (Rule fun c cats) | not (isCoercion fun) = unlines
  [
   "  public void visit" ++ fun ++ "(" ++ packageAbsyn ++ "." ++ fun +++ fnm ++ ")",
   "  {",
   "    /* Code For " ++ fun ++ " Goes Here */",
   "",
   cats' ++ "  }"
  ]
   where
    cats' = if allTerms cats
        then ""
    	else (concatMap (prCat user fnm) (fixOnes (numVars [] cats)))
    allTerms [] = True
    allTerms ((Left z):zs) = False
    allTerms (z:zs) = allTerms zs
    fnm = map toLower fun
prRule user nm _ = ""

--Traverses a class's instance variables.
prCat user fnm c = 
 case c of
  (Right t) -> ""
  (Left nt) -> if isBasic user nt
       then "    visit" ++ (funName nt) ++ "(" ++ fnm ++ "." ++ nt ++ ");\n"
       else if "list" `isPrefixOf` nt
         then "    if (" ++ fnm ++ "." ++ nt ++ " != null) {" ++ accept ++ "}\n"
	 else "    " ++ accept ++ "\n"
      where
       accept = fnm ++ "." ++ nt ++ ".accept(this);"

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
    else (toUpper (head v)) : (init (tail v)) --User-defined type
