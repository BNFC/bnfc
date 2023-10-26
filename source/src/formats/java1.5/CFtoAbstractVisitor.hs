{-
    BNF Converter: Java 1.5 Abstract Vistor generator
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

module CFtoAbstractVisitor (cf2AbstractVisitor) where

import CF
import CFtoJavaAbs15 (typename)
import Utils ((+++), (++++))
import NamedVariables
import Data.List
import Data.Char(toLower, toUpper, isDigit)

cf2AbstractVisitor :: String -> String -> CF -> String
cf2AbstractVisitor packageBase packageAbsyn cf =
  unlines [
      "package" +++ packageBase ++ ";",
      "import" +++ packageAbsyn ++ ".*;",
      "/** BNFC-Generated Abstract Visitor */",
      "public class AbstractVisitor<R,A> implements AllVisitor<R,A> {",
      concatMap (prData packageAbsyn user) groups,
      "}"]
  where
    user = fst (unzip (tokenPragmas cf))
    groups = [ g | g@(c,_) <- fixCoercions (ruleGroupsInternals cf), not (isList c) ]

--Traverses a category based on its type.
prData :: String -> [UserDef] -> (Cat, [Rule]) -> String
prData packageAbsyn user (cat, rules) =
    unlines $ ["/* " ++ identCat cat ++ " */"]
              ++ map (prRule packageAbsyn user cat) rules
              ++ ["    public R visitDefault(" ++ q ++ " p, A arg) {",
                  "      throw new IllegalArgumentException(this.getClass().getName() + \": \" + p);",
                  "    }"]
  where q = packageAbsyn ++ "." ++ identCat cat

--traverses a standard rule.
prRule :: String -> [UserDef] -> Cat -> Rule -> String
prRule packageAbsyn user cat (fun, (_, cats))
    | not (isCoercion fun || isDefinedRule fun) =
   "    public R visit(" ++ cls ++ " p, A arg) { return visitDefault(p, arg); }"
   where cls = packageAbsyn ++ "." ++ fun
prRule  _ _ _ _ = ""
