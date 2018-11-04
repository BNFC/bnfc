{-# LANGUAGE NoImplicitPrelude #-}

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
    Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1335, USA
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
module BNFC.Backend.Java.CFtoVisitSkel15 (cf2VisitSkel) where

import Prelude'

import BNFC.CF
import BNFC.Backend.Java.CFtoJavaAbs15 (typename)
import BNFC.Utils ((+++))
import BNFC.Backend.Common.NamedVariables
import Text.PrettyPrint
import Data.Either (lefts)

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
    groups = fixCoercions (ruleGroupsInternals cf)
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
prData packageAbsyn user (cat, rules)
    | isList cat = ""
    | otherwise = unlines
        ["  public class " ++ identCat cat ++ "Visitor<R,A> implements "
            ++ identCat cat ++ ".Visitor<R,A>"
        , "  {"
        , concatMap (render . nest 4 . prRule packageAbsyn user) rules
        , "  }"
        ]

-- | traverses a standard rule.
-- >>> prRule "ABSYN" [] (Rule "EInt" undefined [Left (TokenCat "Integer"), Left (Cat "NT")])
-- public R visit(ABSYN.EInt p, A arg)
-- { /* Code For EInt Goes Here */
--   //p.integer_;
--   p.nt_.accept(new NTVisitor<R,A>(), arg);
--   return null;
-- }
--
-- It skips the internal category (indicating that a rule is not parsable)
-- >>> prRule "ABSYN" [] (Rule "EInt" undefined [Left (InternalCat), Left (TokenCat "Integer")])
-- public R visit(ABSYN.EInt p, A arg)
-- { /* Code For EInt Goes Here */
--   //p.integer_;
--   return null;
-- }
prRule :: String -> [UserDef] -> Rule -> Doc
prRule packageAbsyn user (Rule fun _ cats)
  | not (isCoercion fun || isDefinedRule fun) = vcat
    [ "public R visit(" <> text packageAbsyn <> "." <> fname <> " p, A arg)"
    , "{"
    , nest 2 ( "/* Code For " <> fname <> " Goes Here */"
            $$ vcat (map (prCat user) cats')
            $$ "return null;" )
    , "}" ]
  where
    fname = text fun                            -- function name
    cats' = filter ((/= InternalCat).fst) (lefts (numVars cats))  -- non-terminals in the rhs
prRule _ _ _ = ""

-- | Traverses a class's instance variables.
-- >>> prCat [] (Cat "A", "a_")
-- p.a_.accept(new AVisitor<R,A>(), arg);
-- >>> prCat [] (TokenCat "Integer", "integer_")
-- //p.integer_;
-- >>> prCat [Cat "A"] (TokenCat "A", "a_")
-- //p.a_;
-- >>> prCat [Cat "A"] (TokenCat "A", "a_2")
-- //p.a_2;
-- >>> prCat [] (ListCat (Cat "A"), "lista_")
-- for (A x: p.lista_)
-- { /* ... */ }
prCat :: [UserDef]    -- ^ User defined tokens
      -> (Cat, Doc)   -- ^ Variable category and name
      -> Doc          -- ^ Code for visiting the variable
prCat user (cat, nt)
  | isTokenCat cat = "//" <> var <> ";"
  | isList cat           = "for" <+> parens (text et <+> "x:" <+> var)
                        $$ braces " /* ... */ "
  | otherwise            = accept
  where
      var = "p." <> nt
      varType = typename (identCat (normCat cat)) user
      accept = var <> ".accept(new " <> text varType <> "Visitor<R,A>(), arg);"
      et = typename (show $normCatOfList cat) user
