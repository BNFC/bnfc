{-
    BNF Converter: Java 1.5 Fold Vistor generator
    Copyright (C) 2006 Bjorn Bringert
    Based on CFtoVisitSkel.hs, Copyright (C) 2004-2006  Michael Pellauer

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

module BNFC.Backend.Java.CFtoFoldVisitor (cf2FoldVisitor) where

import CF
import BNFC.Backend.Java.CFtoJavaAbs15 (typename)
import Utils ((+++), (++++))
import NamedVariables
import Data.List
import Data.Char(toLower, toUpper, isDigit)

cf2FoldVisitor :: String -> String -> CF -> String
cf2FoldVisitor packageBase packageAbsyn cf = 
  unlines
    ["package" +++ packageBase ++ ";",
     "",
     "import" +++ packageAbsyn ++ ".*;",
     "import java.util.Collections;",
     "import java.util.List;",
     "import java.util.ArrayList;",
     "",
     "/** BNFC-Generated Fold Visitor */",
     "public abstract class FoldVisitor<R,A> implements AllVisitor<R,A> {",
     "    public abstract R leaf(A arg);",
     "    public abstract R combine(R x, R y, A arg);",
     "",
     concatMap (prData packageAbsyn user) groups,
     "}"]
  where
    user = fst (unzip (tokenPragmas cf))
    groups = [ g | g@(c,_) <- fixCoercions (ruleGroupsInternals cf), not (isList c) ]

--Traverses a category based on its type.
prData :: String -> [UserDef] -> (Cat, [Rule]) -> String
prData packageAbsyn user (cat, rules) = 
    unlines [
             "/* " ++ identCat cat ++ " */",
	     concatMap (prRule packageAbsyn user cat) rules
	    ]

--traverses a standard rule.
prRule :: String -> [UserDef] -> Cat -> Rule -> String
prRule packageAbsyn user cat (Rule fun _ cats)
    | not (isCoercion fun || isDefinedRule fun) = unlines $
  ["    public R visit(" ++ cls ++ " p, A arg) {",
   "      R r = leaf(arg);"] 
  ++  map ("      "++) visitVars
  ++ ["      return r;",
      "    }"]
   where
    cats' = if allTerms cats
        then []
    	else [ (c,v) | 
	       (Left c, Left v) <- zip cats (fixOnes (numVars [] cats)), c /= internalCat ]
    cls = packageAbsyn ++ "." ++ fun
    allTerms [] = True
    allTerms ((Left z):zs) = False
    allTerms (z:zs) = allTerms zs
    children = map snd cats'
    visitVars = concatMap (uncurry (prCat user)) cats'
prRule  _ _ _ _ = ""

--Traverses a class's instance variables.
prCat :: [UserDef] 
      -> Cat       -- ^ Variable category
      -> String    -- ^ Variable name
      -> [String]  -- ^ Code for visiting the variable
prCat user cat nt 
    | isBasicType user varType || (isListType varType && isBasicType user et) = []
    | isListType varType = listAccept
    | otherwise = ["r = combine(" ++ var ++ ".accept(this, arg), r, arg);"]
      where
      var = "p." ++ nt
      varType = typename (normCat (identCat cat)) user
      et = typename (normCatOfList cat) user
      listAccept = ["for (" ++ et ++ " x : " ++ var ++ ") {", 
                    "  r = combine(x.accept(this,arg), r, arg);",
	            "}"]

isListType :: String -> Bool
isListType nt = "List" `isPrefixOf` nt

--Just checks if something is a basic or user-defined type.
isBasicType :: [UserDef] -> String -> Bool
isBasicType user v = v `elem` (user ++ ["Integer","Character","String","Double"])

