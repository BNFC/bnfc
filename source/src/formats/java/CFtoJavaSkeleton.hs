{-
   **************************************************************
    BNF Converter Module

    Description   : This module generates the Skeleton Function
                    using Appel's method, not the Visitor Pattern.

    Author        : Michael Pellauer (pellauer@cs.chalmers.se)

    License       : GPL (GNU General Public License)

    Created       : 29 April, 2003

    Modified      : 2 September, 2003


   **************************************************************
-}
module CFtoJavaSkeleton (cf2JavaSkeleton) where

import BNFC.CF
import BNFC.Utils ((+++), (++++))
import BNFC.Backend.Common.NamedVariables
import Data.List
import Data.Char(toLower)


cf2JavaSkeleton :: String -> String -> CF -> String
cf2JavaSkeleton packageBase packageAbsyn cf =
  unlines [
    header,
    unlines (map (prData packageAbsyn) groups),
    footer]
  where
    groups = (fixCoercions (ruleGroups cf))
    header = unlines [
      "package" +++ packageBase ++ ";",
      "import" +++ packageAbsyn ++ ".*;",
      "/*** BNFC-Generated Skeleton function. ***/",
      "/* You will probably want to save this in a new file.",
      "   Then do two search-and-replaces.",
      "   First replace \"skel\" with a real funciton name.",
      "   Then replace Object with a real return type. */",
      "",
      "public class Skeleton",
      "{"
      ]
    footer = unlines [ --later only include used categories
      "  public static Object skel(Integer i) { return null; }",
      "  public static Object skel(Double d) { return null; }",
      "  public static Object skel(String s) { return null; }",
      "}"
      ]

--Traverses a category based on its type.
prData :: String -> (Cat, [Rule]) -> String
prData packageAbsyn (cat, rules) =
 if isList cat
 then unlines
 [
  "  public static Object skel(" ++ packageAbsyn ++ "." ++ identCat (normCat cat) +++ "foo)",
  "  {",
  prList cat rules,
  "  }"
 ]
 else unlines --not a list
 [
  "  public static Object skel(" ++ packageAbsyn ++ "." ++ identCat (normCat cat) +++ "foo)",
  "  {",
  unlines (map (prRule packageAbsyn) rules),
  "    return null;",
  "  }"
 ]

--traverses a standard rule
prRule :: String -> Rule -> String
prRule packageAbsyn (Rule fun c cats) | not (isCoercion fun) = unlines
  [
   "    if (foo instanceof" +++ packageAbsyn ++ "." ++ fun ++ ")",
   "    {",
   "       " ++ packageAbsyn ++ "." ++ fun +++ fnm +++ "= ("
     ++ packageAbsyn ++ "." ++ fun ++ ") foo;",
   "",
   "       /* Code For " ++ fun ++ " Goes Here */",
   "",
   cats',
   "       return null;",
   "    }"
  ]
   where
    cats' = if allTerms cats
        then ""
    	else (unlines (map (prCat fnm) (fixOnes (numVars [] cats))))
    allTerms [] = True
    allTerms ((Left z):zs) = False
    allTerms (z:zs) = allTerms zs
    fnm = map toLower fun
prRule nm _ = ""

--This traverses list rules.
prList :: Cat -> [Rule] -> String
prList c rules = unlines
  [
   "    if (foo == null)",
   "    {",
   "       /* Optional End of List Code Goes Here */",
   "       return null;",
   "    }",
   "    else",
   "    {",
   "      /* Optional List Member Code Goes Here */",
   "      skel(foo." ++ c' ++ "_);",
   "      skel(foo." ++ c'' ++ "_);",
   "      return null;",
   "    }"
  ]
 where
    c' = map toLower (identCat (normCatOfList c))
    c'' = map toLower (identCat c)

--This traverses a class's instance variables.
prCat fnm c = case c of
		(Right t) -> ""
		(Left nt) -> "       skel(" ++ fnm ++ "." ++ nt ++ ");"
