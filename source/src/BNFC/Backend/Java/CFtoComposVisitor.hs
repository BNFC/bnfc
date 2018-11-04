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

import Data.List
import Data.Either (lefts)
import BNFC.CF
import BNFC.Backend.Java.CFtoJavaAbs15 (typename)
import BNFC.Utils ((+++))
import BNFC.Backend.Common.NamedVariables
import BNFC.PrettyPrint

cf2ComposVisitor :: String -> String -> CF -> String
cf2ComposVisitor packageBase packageAbsyn cf =
  concat [
    header,
    concatMap (prData packageAbsyn user) groups,
    "}"]
  where
    user   = fst (unzip (tokenPragmas cf))
    groups =
        [ g
        | g@(c,_) <- fixCoercions (ruleGroupsInternals cf)
        , not (isList c)
        ]
    is     = map (prInterface packageAbsyn) groups
    header = unlines
      [ "package" +++ packageBase ++ ";"
      , "import" +++ packageAbsyn ++ ".*;"
      , "/** BNFC-Generated Composition Visitor"
      , "*/"
      , ""
      , "public class ComposVisitor<A> implements"
      , intercalate ",\n" $ map ("  "++) is
      , "{"
      ]


prInterface :: String -> (Cat, [Rule]) -> String
prInterface packageAbsyn (cat, _) =
    q ++ ".Visitor<" ++ q ++ ",A>"
  where q = packageAbsyn ++ "." ++ identCat cat

-- | Traverses a category based on its type.

prData :: String -> [UserDef] -> (Cat, [Rule]) -> String
prData packageAbsyn user (cat, rules) = unlines
    [ "/* " ++ identCat cat ++ " */"
    , concatMap (render . prRule packageAbsyn user cat) rules
    ]

-- | Traverses a standard rule.
--
-- >>> prRule "lang.absyn" [Cat "A"] (Cat "B") (Rule "F" (Cat "B") [Left (Cat "A"), Right "+", Left (ListCat (Cat "B"))])
--     public B visit(lang.absyn.F p, A arg)
--     {
--       String a_ = p.a_;
--       ListB listb_ = new ListB();
--       for (B x : p.listb_)
--       {
--         listb_.add(x.accept(this,arg));
--       }
--       return new lang.absyn.F(a_, listb_);
--     }

prRule :: String -> [UserDef] -> Cat -> Rule -> Doc
prRule packageAbsyn user cat (Rule fun _ cats)
  | not (isCoercion fun || isDefinedRule fun) = nest 4 $ vcat
    [ "public " <> text(identCat cat) <> " visit(" <> cls <> " p, A arg)"
    , codeblock 2
        [ vcat (map (prCat user) cats')
        , "return new" <+> cls <> parens (hsep (punctuate "," vnames)) <> ";" ] ]
  where
    cats' = filter ((/= InternalCat) . fst) (lefts (numVars cats))
    cls = text (packageAbsyn ++ "." ++ fun)
    vnames = map snd cats'
prRule  _ _ _ _ = ""

-- | Traverses a class's instance variables.
--
-- >>> prCat [Cat "A"] (Cat "A", "a_")
-- String a_ = p.a_;
--
-- >>> prCat [] (ListCat (Cat "Integer"), "listinteger_")
-- ListInteger listinteger_ = p.listinteger_;
--
-- >>> prCat [] (ListCat (Cat "N"), "listn_")
-- ListN listn_ = new ListN();
-- for (N x : p.listn_)
-- {
--   listn_.add(x.accept(this,arg));
-- }
--
-- >>> prCat [] (Cat "N", "n_")
-- N n_ = p.n_.accept(this, arg);

prCat :: [UserDef]  -- ^ User defined token categories
      -> (Cat, Doc) -- ^ Variable category and names
      -> Doc        -- ^ Code for visiting the variable
prCat user (cat, nt)
  | isBasicType user varType || (isList cat && isBasicType user et) = decl var
  | isList cat = decl ("new" <+> text varType <> "()")
              $$ "for (" <> text et <> " x : " <> var <> ")"
              $$ codeblock 2 [ nt <> ".add(x.accept(this,arg));" ]
  | otherwise = decl (var <> ".accept(this, arg)")
  where
    var     = "p." <> nt
    varType = typename (identCat (normCat cat)) user
    et      = typename (identCat (normCatOfList cat)) user
    decl v  = text varType <+> nt <+> "=" <+> v <> ";"

-- | Just checks if something is a basic or user-defined type.

isBasicType :: [UserDef] -> String -> Bool
isBasicType user v =
    v `elem` (map show user ++ ["Integer","Character","String","Double"])
