{-
    BNF Converter: Java 1.5 All Visitor generator
    Copyright (C) 2006 Bjorn Bringert
    Based on CFtoVisitSkel.hs, Copyright (C) 2004-2006  Michael Pellauer

-}

module BNFC.Backend.Java.CFtoAllVisitor (cf2AllVisitor) where

import Data.List (intercalate)
import BNFC.CF
import BNFC.Utils ((+++))
import BNFC.Backend.Common.NamedVariables


cf2AllVisitor :: String -> String -> CF -> String
cf2AllVisitor packageBase packageAbsyn cf = unlines $ concat
  [ [ "package" +++ packageBase ++ ";"
    , ""
    , "/** All Visitor */"
    , ""
    , "public interface AllVisitor<R,A>" ++ if null is then "" else " extends"
    ]
  , [ intercalate ",\n" $ map ("  "++) is | not $ null is ]
  , [ "{}" ]
  ]
  where
    groups = [ g
        | g@(c,_) <- fixCoercions (ruleGroupsInternals cf), not (isList c) ]
    is     = map (prInterface packageAbsyn) groups

prInterface :: String -> (Cat, [Rule]) -> String
prInterface packageAbsyn (cat, _) =
    q ++ ".Visitor<R,A>"
  where q = packageAbsyn ++ "." ++ identCat cat
