{-
    BNF Converter: Java Vistor skeleton generator
    Copyright (C) 2004  Author:  Michael Pellauer, Bjorn Bringert

    Description   : This module generates a Skeleton function
                    which uses the Visitor Design Pattern, which
                    users may find more familiar than Appel's
                    method.

    Author        : Michael Pellauer
                    Bjorn Bringert
    Created       : 4 August, 2003
    Modified      : 16 June, 2004

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module BNFC.Backend.Java.CFtoVisitSkel15 (cf2VisitSkel) where

import Data.Bifunctor   ( second )
import Data.Either      ( lefts  )
import Text.PrettyPrint
import qualified Text.PrettyPrint as P

import BNFC.CF
import BNFC.Utils       ( (+++) )

import BNFC.Backend.Common.NamedVariables
import BNFC.Backend.Java.CFtoJavaAbs15    ( typename )

--Produces a Skeleton using the Visitor Design Pattern.
--Thus the user can choose which Skeleton to use.

cf2VisitSkel :: String -> String -> CF -> String
cf2VisitSkel packageBase packageAbsyn cf =
  concat [
    header,
    concatMap (prData packageAbsyn user) groups,
    "}"]
  where
    user   = fst $ unzip $ tokenPragmas cf
    groups = fixCoercions $ ruleGroupsInternals cf
    header = unlines [
      "package" +++ packageBase ++ ";",
      "",
      "/*** Visitor Design Pattern Skeleton. ***/",
      "",
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
prData packageAbsyn user (cat, rules)
    | isList cat = ""
    | otherwise = unlines
        ["  public class " ++ identCat cat ++ "Visitor<R,A> implements "
            ++ qual (identCat cat) ++ ".Visitor<R,A>"
        , "  {"
        , render $ vcat $ map (nest 4 . prRule packageAbsyn user) rules
        , "  }"
        ]
  where
  qual x = packageAbsyn ++ "." ++ x

-- | traverses a standard rule.
-- >>> prRule "ABSYN" [] $ Rule "EInt" undefined [Left (TokenCat "Integer"), Left (Cat "NT")] Parsable
-- public R visit(ABSYN.EInt p, A arg)
-- { /* Code for EInt goes here */
--   //p.integer_;
--   p.nt_.accept(new NTVisitor<R,A>(), arg);
--   return null;
-- }
--
-- It skips the internal category (indicating that a rule is not parsable)
-- >>> prRule "ABSYN" [] $ Rule "EInt" undefined [Left (TokenCat "Integer")] Internal
-- public R visit(ABSYN.EInt p, A arg)
-- { /* Code for EInt goes here */
--   //p.integer_;
--   return null;
-- }
prRule :: IsFun f => String -> [UserDef] -> Rul f -> Doc
prRule packageAbsyn user (Rule fun _ cats _)
  | not (isCoercion fun || isDefinedRule fun) = vcat
    [ "public R visit(" P.<> text packageAbsyn P.<> "." P.<> fname P.<> " p, A arg)"
    , "{"
    , nest 2 $ vcat
        [ "/* Code for " P.<> fname P.<> " goes here */"
        , vcat $ map (prCat packageAbsyn user) cats'
        , "return null;"
        ]
    , "}"
    ]
  where
    fname = text $ funName fun              -- function name
    cats' = map (second ("p." P.<>)) $ lefts $ numVars cats  -- non-terminals in the rhs
prRule _ _ _ = empty

-- | Traverses a class's instance variables.
--
-- >>> prCat "ABSYN" [] (Cat "A", "p.a_")
-- p.a_.accept(new AVisitor<R,A>(), arg);
--
-- >>> prCat "" [] (TokenCat "Integer", "p.integer_")
-- //p.integer_;
--
-- >>> prCat "" ["A"] (TokenCat "A", "p.a_")
-- //p.a_;
--
-- >>> prCat "" ["A"] (TokenCat "A", "p.a_2")
-- //p.a_2;
--
-- >>> prCat "ABSYN" [] (ListCat (Cat "A"), "p.lista_")
-- for (ABSYN.A x: p.lista_) {
--   x.accept(new AVisitor<R,A>(), arg);
-- }
prCat :: String       -- ^ absyn package name.
      -> [UserDef]    -- ^ User defined tokens.
      -> (Cat, Doc)   -- ^ Variable category and name.
      -> Doc          -- ^ Code for visiting the variable.
prCat packageAbsyn user (cat, var) =
  case cat of
    TokenCat{}   -> "//" P.<> var P.<> ";"
    ListCat cat' -> vcat
      [ "for" <+> parens (text et <+> "x:" <+> var) <+> "{"
      , nest 2 $ prCat packageAbsyn user (cat', "x")
      , "}"
      ]
    _ -> var P.<> ".accept(new " P.<> text varType P.<> "Visitor<R,A>(), arg);"
  where
    varType = typename "" user $ identCat $ normCat cat    -- no qualification here!
    et      = typename packageAbsyn user $ identCat $ normCatOfList cat
