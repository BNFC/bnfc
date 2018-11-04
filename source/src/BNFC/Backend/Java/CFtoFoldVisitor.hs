{-# LANGUAGE NoImplicitPrelude #-}

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
    Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1335, USA
-}

module BNFC.Backend.Java.CFtoFoldVisitor (cf2FoldVisitor) where

import Prelude'

import BNFC.CF
import BNFC.Backend.Java.CFtoJavaAbs15 (typename)
import BNFC.Utils ((+++))
import BNFC.Backend.Common.NamedVariables
import Data.Either (lefts)
import BNFC.PrettyPrint

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
prData packageAbsyn user (cat, rules) = unlines
    [ "/* " ++ identCat cat ++ " */"
    , concatMap (prRule packageAbsyn user cat) rules
    ]

--traverses a standard rule.
prRule :: String -> [UserDef] -> Cat -> Rule -> String
prRule packageAbsyn user _ (Rule fun _ cats)
    | not (isCoercion fun || isDefinedRule fun) = unlines $
  ["    public R visit(" ++ cls ++ " p, A arg) {",
   "      R r = leaf(arg);"]
  ++  map ("      "++) visitVars
  ++ ["      return r;",
      "    }"]
   where
    cats' = filter ((/= InternalCat) . fst) (lefts (numVars cats))
    cls = packageAbsyn ++ "." ++ fun
    visitVars = lines $ render $ vcat $ map (prCat user) cats'
prRule  _ _ _ _ = ""

-- | Traverses a class's instance variables.
-- >>> prCat [Cat "A"] (Cat "A", "a_")
-- <BLANKLINE>
-- >>> prCat [] (ListCat (Cat "Integer"), "listinteger_")
-- <BLANKLINE>
-- >>> prCat [] (ListCat (Cat "N"), "listn_")
-- for (N x : p.listn_)
-- {
--   r = combine(x.accept(this, arg), r, arg);
-- }
-- >>> prCat [] (Cat "N", "n_")
-- r = combine(p.n_.accept(this, arg), r, arg);
prCat :: [UserDef]
      -> (Cat, Doc) -- ^ Variable category and name
      -> Doc        -- ^ Code for visiting the variable
prCat user (cat,nt)
    | isBasicType user varType || (isList cat && isBasicType user et) = empty
    | isList cat = vcat
        [ "for (" <> text et <> " x : " <> var <> ")"
        , codeblock 2 [ "r = combine(x.accept(this, arg), r, arg);" ] ]
    | otherwise = "r = combine(" <> var <> ".accept(this, arg), r, arg);"
      where
      var = "p." <> nt
      varType = typename (identCat (normCat cat)) user
      et      = typename (show$normCatOfList cat) user

--Just checks if something is a basic or user-defined type.
isBasicType :: [UserDef] -> String -> Bool
isBasicType user v = v `elem` (map show user ++ ["Integer","Character","String","Double"])
