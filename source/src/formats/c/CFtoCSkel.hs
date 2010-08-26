{-
    BNF Converter: C Skeleton generator
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

    Description   : This module generates the C Skeleton functions.
    
                    The generated files follow Appel's case method.

    Author        : Michael Pellauer (pellauer@cs.chalmers.se)

    License       : GPL (GNU General Public License)

    Created       : 9 August, 2003                           

    Modified      : 12 August, 2003                          

   
   ************************************************************** 
-}

module CFtoCSkel (cf2CSkel) where

import CF
import Utils			( (+++) )
import NamedVariables
import List			( isPrefixOf )
import Char			( toLower, toUpper )

--Produces (.H file, .C file)
cf2CSkel :: CF -> (String, String)
cf2CSkel cf = (mkHFile cf groups, mkCFile cf groups)
 where
    groups = (fixCoercions (ruleGroups cf))


{- **** Header (.H) File Functions **** -}

--Generates the Header File
mkHFile :: CF -> [(Cat,[Rule])] -> String
mkHFile cf groups = unlines
 [
  header,
  concatMap prDataH groups,
  concatMap prUserH user,
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
    "#include \"Absyn.h\"",
    ""
   ]
  prUserH u = "void visit" ++ u' ++ "(" ++ u ++ " p);"
    where
     u' = ((toUpper (head u)) : (map toLower (tail u))) --this is a hack to fix a potential capitalization problem.
  footer = unlines
   [
    "void visitIdent(Ident i);",
    "void visitInteger(Integer i);",
    "void visitDouble(Double d);",
    "void visitChar(Char c);",
    "void visitString(String s);",
    "",
    "#endif"
   ]
    
--Prints out visit functions for a category
prDataH :: (Cat, [Rule]) -> String
prDataH (cat, _rules) = 
    if isList cat
      then concat ["void visit", cl, "(", cl,  " p);\n"]
      else "void visit" ++ cl ++ "(" ++ cl ++ " p);\n"
    where cl = identCat $ normCat cat

{- **** Implementation (.C) File Functions **** -}

--Makes the .C File
mkCFile :: CF -> [(Cat,[Rule])] -> String
mkCFile cf groups = concat 
   [
    header,
    concatMap (prData user) groups,
    concatMap prUser user,
    footer
   ]
  where
    user = fst (unzip (tokenPragmas cf))
    header = unlines [
      "/*** BNFC-Generated Visitor Traversal Skeleton. ***/",
      "/* This traverses the abstract syntax tree.",
      "   To use, copy Skeleton.h and Skeleton.c to",
      "   new files. */",
      "",
      "#include \"Skeleton.h\"",
      ""
      ]
    prUser u = unlines
     [
      "void visit" ++ u' ++ "(" ++ u ++ " p)",
      "{",
      "  /* Code for " ++ u ++ " Goes Here */",
      "}"
     ]
     where
      u' = ((toUpper (head u)) : (map toLower (tail u))) --this is a hack to fix a potential capitalization problem.
    footer = unlines 
     [
      "void visitIdent(Ident i)",
      "{",
      "  /* Code for Ident Goes Here */",
      "}",
      "void visitInteger(Integer i)",
      "{",
      "  /* Code for Integer Goes Here */",
      "}",
      "void visitDouble(Double d)",
      "{",
      "  /* Code for Double Goes Here */",
      "}",
      "void visitChar(Char c)",
      "{",
      "  /* Code for Char Goes Here */",
      "}",
      "void visitString(String s)",
      "{",
      "  /* Code for String Goes Here */",
      "}",
      ""
     ]

--Visit functions for a category.
prData :: [UserDef] -> (Cat, [Rule]) -> String
prData user (cat, rules) = 
    if isList cat
      then unlines
	       [
		"void visit" ++ cl ++ "("++ cl +++ vname ++ ")",
		"{",
		"  while(" ++ vname ++ " != 0)",
		"  {",
		"    /* Code For " ++ cl ++ " Goes Here */",
		"    visit" ++ ecl ++ "(" ++ vname ++ "->" ++ member ++ "_);",
		"    " ++ vname +++ "=" +++ vname ++ "->" ++ vname ++ "_;",
		"  }",
		"}",
		""
	       ]
      -- Not a list:
      else unlines
	       [
		"void visit" ++ cl ++ "(" ++ cl ++ " _p_)",
		"{",
		"  switch(_p_->kind)",
		"  {",
		concatMap (prPrintRule user) rules,
		"  default:",
		"    fprintf(stderr, \"Error: bad kind field when printing " ++ cl ++ "!\\n\");",
		"    exit(1);",
		"  }",
		"}\n"
	       ]
    where cl = identCat $ normCat cat
	  ecl = identCat $ normCatOfList cat
	  vname = map toLower cl
	  member = map toLower ecl

-- Visits all the instance variables of a category.
prPrintRule :: [UserDef] -> Rule -> String
prPrintRule user (fun, (_c, cats)) | not (isCoercion fun) = unlines
  [
   "  case is_" ++ fun ++ ":",
   "    /* Code for " ++ fun ++ " Goes Here */",
   cats' ++ "    break;"
  ]
   where
    cats' = concatMap (prCat user fun) (zip (fixOnes (numVars [] cats)) cats)
prPrintRule _user (_fun, _cats) = ""

-- Prints the actual instance-variable visiting.
prCat :: [UserDef] -> String -> (Either Cat Cat, Either Cat Cat) -> String
prCat user fnm (c, o) = 
    case c of
      Right {} -> ""
      Left nt  ->
        if isBasic user nt
	  then "    visit" ++ basicFunName nt ++ "(_p_->u." ++ v ++ "_." ++ nt ++ ");\n"
	  else "    visit" ++ o' ++ "(_p_->u." ++ v ++ "_." ++ nt ++ ");\n"
    where v = map toLower $ identCat $ normCat fnm
	  o' = case o of
	         Right x -> x
		 Left x  -> normCat $ identCat x

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
basicFunName :: String -> String
basicFunName v = 
    if "integer_" `isPrefixOf` v then "Integer"
    else if "char_" `isPrefixOf` v then "Char"
    else if "string_" `isPrefixOf` v then "String"
    else if "double_" `isPrefixOf` v then "Double"
    else if "ident_" `isPrefixOf` v then "Ident"
    else (toUpper (head v)) : (init (tail v)) --User-defined type
