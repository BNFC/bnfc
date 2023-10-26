{-
    BNF Converter: Java Vistor skeleton generator
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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{-
   **************************************************************
    BNF Converter Module

    Description   : This module generates a Skeleton function
                    which uses the Visitor Design Pattern, which
                    users may find more familiar than Appel's
                    method.

    Author        : Michael Pellauer (pellauer@cs.chalmers.se),
                    Bjorn Bringert (bringert@cs.chalmers.se)

    License       : GPL (GNU General Public License)

    Created       : 4 August, 2003

    Modified      : 16 June, 2004


   **************************************************************
-}
module CFtoVisitSkel15 (cf2VisitSkel) where

import CF
import CFtoJavaAbs15 (typename)
import Utils ((+++), (++++))
import NamedVariables
import Data.List
import Data.Char(toLower, toUpper, isDigit)

--Produces a Skeleton using the Visitor Design Pattern.
--Thus the user can choose which Skeleton to use.

cf2VisitSkel :: String -> String -> CF -> String
cf2VisitSkel packageBase packageAbsyn cf =
  concat [
    header,
--    "  // NOT IMPLEMENTED for java1.5\n",
    concatMap (prData packageAbsyn user) groups,
    "}"]
  where
    user = fst (unzip (tokenPragmas cf))
    groups = (fixCoercions (ruleGroups cf))
    header = unlines [
      "package" +++ packageBase ++ ";",
      "import" +++ packageAbsyn ++ ".*;",
      "/*** BNFC-Generated Visitor Design Pattern Skeleton. ***/",
      "/* This implements the common visitor design pattern.",
      "   Tests show it to be slightly less efficient than the",
      "   instanceof method, but easier to use. ",
      "   Replace the R and A parameters with the desired return",
      "   and context types.*/",
      "",
      "public class VisitSkel",
      "{"
      ]


--Traverses a category based on its type.
prData :: String -> [UserDef] -> (Cat, [Rule]) -> String
prData packageAbsyn user (cat, rules) =
 if isList cat
 then ""
 else unlines ["  public class " ++ identCat cat ++ "Visitor<R,A> implements "
	       ++ identCat cat ++ ".Visitor<R,A>",
	       "  {",
	       concatMap (prRule packageAbsyn user) rules,
	       "  }"
	      ]

--traverses a standard rule.
prRule :: String -> [UserDef] -> Rule -> String
prRule packageAbsyn user (fun, (c, cats)) | not (isCoercion fun || isDefinedRule fun) = unlines
  [
   "    public R visit(" ++ packageAbsyn ++ "." ++ fun ++ " p, A arg)",
   "    {",
   "      /* Code For " ++ fun ++ " Goes Here */",
   "",
   concatMap (uncurry (prCat user)) cats',
   "      return null;",
   "    }"
  ]
   where
    cats' = if allTerms cats
        then []
    	else [ (c,v) |
	       (Left c, Left v) <- zip cats (fixOnes (numVars [] cats)) ]
    allTerms [] = True
    allTerms ((Left z):zs) = False
    allTerms (z:zs) = allTerms zs
prRule user nm (fun, cats) = ""

--Traverses a class's instance variables.
prCat :: [UserDef]
      -> Cat       -- ^ Variable category
      -> String    -- ^ Variable name
      -> String    -- ^ Code for visiting the variable
prCat user cat nt | isBasic user nt = "      //" ++ var ++ ";\n"
		  | "list" `isPrefixOf` nt = listAccept
		  | otherwise = "      " ++ accept ++ "\n"
      where
      var = "p." ++ nt
      varType = typename (normCat (identCat cat)) user
      accept = var ++ ".accept(new " ++ varType ++ "Visitor<R,A>(), arg);"
      et = typename (normCatOfList cat) user
      listAccept = unlines ["      for (" ++ et ++ " x : " ++ var ++ ") {",
			    "      }"]

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
