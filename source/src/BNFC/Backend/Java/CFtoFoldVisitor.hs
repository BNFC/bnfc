{-
    BNF Converter: Java 1.5 Fold Vistor generator
    Copyright (C) 2006 Bjorn Bringert
    Based on CFtoVisitSkel.hs, Copyright (C) 2004-2006  Michael Pellauer

-}

{-# LANGUAGE OverloadedStrings #-}

module BNFC.Backend.Java.CFtoFoldVisitor (cf2FoldVisitor) where

import Prelude hiding ((<>))

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
     "/** Fold Visitor */",
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
prRule packageAbsyn user _ (Rule fun _ cats _)
    | not (isCoercion fun || isDefinedRule fun) = unlines $
  ["    public R visit(" ++ cls ++ " p, A arg) {",
   "      R r = leaf(arg);"]
  ++  map ("      "++) visitVars
  ++ ["      return r;",
      "    }"]
   where
    cats' = lefts $ numVars cats
    cls = packageAbsyn ++ "." ++ funName fun
    visitVars = lines $ render $ vcat $ map (prCat packageAbsyn user) cats'
prRule  _ _ _ _ = ""

-- | Traverses a class's instance variables.
-- >>> prCat "" ["A"] (Cat "A", "a_")
-- <BLANKLINE>
-- >>> prCat "" [] (ListCat (Cat "Integer"), "listinteger_")
-- <BLANKLINE>
-- >>> prCat "absyn" [] (ListCat (Cat "N"), "listn_")
-- for (absyn.N x : p.listn_)
-- {
--   r = combine(x.accept(this, arg), r, arg);
-- }
-- >>> prCat "absyn" [] (Cat "N", "n_")
-- r = combine(p.n_.accept(this, arg), r, arg);
prCat :: String     -- ^ Absyn package name.
      -> [UserDef]  -- ^ User-defined token categories.
      -> (Cat, Doc) -- ^ Variable category and name
      -> Doc        -- ^ Code for visiting the variable
prCat packageAbsyn user (cat,nt)
    | isBasicType user varType || (isList cat && isBasicType user et) = empty
    | isList cat = vcat
        [ "for (" <> text et <> " x : " <> var <> ")"
        , codeblock 2 [ "r = combine(x.accept(this, arg), r, arg);" ] ]
    | otherwise = "r = combine(" <> var <> ".accept(this, arg), r, arg);"
      where
      var = "p." <> nt
      varType = typename packageAbsyn user $ identCat $ normCat cat
      et      = typename packageAbsyn user $ identCat $ normCatOfList cat

--Just checks if something is a basic or user-defined type.
isBasicType :: [UserDef] -> String -> Bool
isBasicType user v = v `elem` (user ++ ["Integer","Character","String","Double"])
