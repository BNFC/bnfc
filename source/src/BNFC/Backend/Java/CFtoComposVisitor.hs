{-# LANGUAGE NoImplicitPrelude #-}

{-
    BNF Converter: Java 1.5 Compositional Vistor generator
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

module BNFC.Backend.Java.CFtoComposVisitor (cf2ComposVisitor) where

import Prelude'

import Data.List   (intercalate)
import Data.Either (lefts)

import BNFC.CF
import BNFC.Backend.Java.CFtoJavaAbs15 (typename)
import BNFC.Utils ((+++))
import BNFC.Backend.Common.NamedVariables
import BNFC.PrettyPrint

cf2ComposVisitor :: String -> String -> CF -> String
cf2ComposVisitor packageBase packageAbsyn cf = concat
  [ header
  , intercalate "\n" $ map (prData packageAbsyn user) groups
  , "}"
  ]
  where
    user   = map fst $ tokenPragmas cf
    groups =
        [ g
        | g@(c,_) <- fixCoercions (ruleGroupsInternals cf)
        , not (isList c)
        ]
    is     = map (prInterface packageAbsyn) groups
    header = unlines $ concat
      [ [ "package" +++ packageBase ++ ";"
        , "/** BNFC-Generated Composition Visitor"
        , "*/"
        , ""
        , "public class ComposVisitor<A>" ++ if null is then "" else " implements"
        ]
      , [ intercalate ",\n" $ map ("  " ++) is | not $ null is ]
      , [ "{" ]
      ]


prInterface :: String -> (Cat, [Rule]) -> String
prInterface packageAbsyn (cat, _) =
    q ++ ".Visitor<" ++ q ++ ",A>"
  where q = packageAbsyn ++ "." ++ identCat cat

-- | Traverses a category based on its type.

prData :: String -> [UserDef] -> (Cat, [Rule]) -> String
prData packageAbsyn user (cat, rules) = unlines
    [ "    /* " ++ identCat cat ++ " */"
    , render $ vcat $ map (prRule packageAbsyn user cat) rules
    ]

-- | Traverses a standard rule.
--
-- >>> prRule "lang.absyn" ["A"] (Cat "B") $ Rule "F" (Cat "B") [Left (Cat "A"), Right "+", Left (ListCat (Cat "B"))] Parsable
--     public lang.absyn.B visit(lang.absyn.F p, A arg)
--     {
--       String a_ = p.a_;
--       lang.absyn.ListB listb_ = new lang.absyn.ListB();
--       for (lang.absyn.B x : p.listb_)
--       {
--         listb_.add(x.accept(this,arg));
--       }
--       return new lang.absyn.F(a_, listb_);
--     }

prRule :: String -> [UserDef] -> Cat -> Rule -> Doc
prRule packageAbsyn user cat (Rule fun _ cats _)
  | not (isCoercion fun || isDefinedRule fun) = nest 4 $ vcat
    [ "public " <> qual (identCat cat) <> " visit(" <> cls <> " p, A arg)"
    , codeblock 2
        [ vcat (map (prCat packageAbsyn user) cats')
        , "return new" <+> cls <> parens (hsep (punctuate "," vnames)) <> ";"
        ]
    ]
  where
    cats'  = lefts $ numVars cats
    cls    = qual fun
    qual s = text (packageAbsyn ++ "." ++ s)
    vnames = map snd cats'
prRule  _ _ _ _ = empty

-- | Traverses a class's instance variables.
--
-- >>> prCat "lang.absyn" ["A"] (Cat "A", "a_")
-- String a_ = p.a_;
--
-- >>> prCat "lang.absyn" [] (ListCat (Cat "Integer"), "listinteger_")
-- lang.absyn.ListInteger listinteger_ = p.listinteger_;
--
-- >>> prCat "lang.absyn" [] (ListCat (Cat "N"), "listn_")
-- lang.absyn.ListN listn_ = new lang.absyn.ListN();
-- for (lang.absyn.N x : p.listn_)
-- {
--   listn_.add(x.accept(this,arg));
-- }
--
-- >>> prCat "lang.absyn" [] (Cat "N", "n_")
-- lang.absyn.N n_ = p.n_.accept(this, arg);

prCat :: String     -- ^ Name of package for abstract syntax.
      -> [UserDef]  -- ^ User defined token categories.
      -> (Cat, Doc) -- ^ Variable category and names.
      -> Doc        -- ^ Code for visiting the variable.
prCat packageAbsyn user (cat, nt)
  | isBasicType user varType || (isList cat && isBasicType user et) = decl var
  | isList cat = vcat
      [ decl ("new" <+> text varType <> "()")
      , "for (" <> text et <> " x : " <> var <> ")"
      , codeblock 2 [ nt <> ".add(x.accept(this,arg));" ]
      ]
  | otherwise = decl (var <> ".accept(this, arg)")
  where
    var     = "p." <> nt
    varType = typename packageAbsyn user $ identCat $ normCat cat
    et      = typename packageAbsyn user $ identCat $ normCatOfList cat
    decl v  = text varType <+> nt <+> "=" <+> v <> ";"
    -- qual s  = text (packageAbsyn ++ "." ++ s)

-- | Just checks if something is a basic or user-defined type.

isBasicType :: [UserDef] -> String -> Bool
isBasicType user v =
    v `elem` (user ++ ["Integer","Character","String","Double"])
