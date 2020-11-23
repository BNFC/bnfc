{-
    BNF Converter: Java 1.5 Abstract Vistor generator
    Copyright (C) 2006 Bjorn Bringert
    Based on CFtoVisitSkel.hs, Copyright (C) 2004-2006  Michael Pellauer

-}

module BNFC.Backend.Java.CFtoAbstractVisitor (cf2AbstractVisitor) where

import BNFC.CF
import BNFC.Utils ((+++))
import BNFC.Backend.Common.NamedVariables

cf2AbstractVisitor :: String -> String -> CF -> String
cf2AbstractVisitor packageBase packageAbsyn cf = unlines
    [ "package" +++ packageBase ++ ";"
    , ""
    , "/** BNFC-Generated Abstract Visitor */"
    , ""
    , "public class AbstractVisitor<R,A> implements AllVisitor<R,A> {"
    , concatMap (prData packageAbsyn user) groups
    , "}"
    ]
  where
    user   = map fst $ tokenPragmas cf
    groups = [ g
      | g@(c,_) <- fixCoercions (ruleGroupsInternals cf), not (isList c) ]

--Traverses a category based on its type.
prData :: String -> [UserDef] -> (Cat, [Rule]) -> String
prData packageAbsyn user (cat, rules) = unlines $ concat
  [ [ "    /* " ++ identCat cat ++ " */" ]
  , concatMap (prRule packageAbsyn user cat) rules
  , [ "    public R visitDefault(" ++ packageAbsyn ++ "." ++ identCat cat ++ " p, A arg) {"
    , "      throw new IllegalArgumentException(this.getClass()" ++ ".getName() + \": \" + p);"
    , "    }"
    ]
  ]

--traverses a standard rule.
prRule :: String -> [UserDef] -> Cat -> Rule -> [String]
prRule packageAbsyn _ _ (Rule fun _ _ _)
  | not (isCoercion fun || isDefinedRule fun) = return $ concat
      [ "    public R visit("
      , packageAbsyn ++ "." ++ funName fun
      , " p, A arg) { return visitDefault(p, arg); }"
      ]
  | otherwise = []
