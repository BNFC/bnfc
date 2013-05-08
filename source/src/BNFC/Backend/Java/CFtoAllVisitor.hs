{-
    BNF Converter: Java 1.5 All Visitor generator
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

module BNFC.Backend.Java.CFtoAllVisitor (cf2AllVisitor) where

import BNFC.CF
import BNFC.Backend.Java.CFtoJavaAbs15 (typename)
import BNFC.Utils ((+++), (++++))
import BNFC.Backend.Common.NamedVariables
import Data.List
import Data.Char(toLower, toUpper, isDigit)

cf2AllVisitor :: String -> String -> CF -> String
cf2AllVisitor packageBase packageAbsyn cf =
  unlines [
           "package" +++ packageBase ++ ";",
           "",
           "import" +++ packageAbsyn ++ ".*;",
           "",
           "/** BNFC-Generated All Visitor */",
           "public interface AllVisitor<R,A> extends",
             concat $ intersperse ",\n" $ map ("  "++) is,
           "{}"]
  where
    groups = [ g | g@(c,_) <- fixCoercions (ruleGroupsInternals cf), not (isList c) ]
    is = map (prInterface packageAbsyn) groups
    header = unlines [

      ]

prInterface :: String -> (Cat, [Rule]) -> String
prInterface packageAbsyn (cat, rules) =
    q ++ ".Visitor<R,A>"
  where q = packageAbsyn ++ "." ++ identCat cat
