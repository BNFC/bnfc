{-# LANGUAGE NoImplicitPrelude #-}

module BNFC.Backend.Haskell.Utils
  ( parserName
  , hsReservedWords
  , catToType
  , catvars
  ) where

import Prelude'

import Text.PrettyPrint
import BNFC.CF (Cat(..), identCat, normCat)
import BNFC.Utils (mkNames, NameStyle(..))

-- | Create a valid parser function name for a given category
-- >>> parserName (Cat "Abcd")
-- pAbcd
-- >>> parserName (ListCat (Cat "Xyz"))
-- pListXyz
parserName :: Cat -> Doc
parserName = ("p" <>) . text . identCat

-- | Haskell's reserved words
hsReservedWords :: [String]
hsReservedWords =
    [ "as"
    , "case"
    , "class"
    , "data"
    , "default"
    , "deriving"
    , "do"
    , "else"
    , "hiding"
    , "if"
    , "import"
    , "in"
    , "infix"
    , "infixl"
    , "infixr"
    , "instance"
    , "let"
    , "module"
    , "newtype"
    , "of"
    , "qualified"
    , "then"
    , "type"
    , "where"
    ]


-- | Render a category from the grammar to a Haskell type
-- >>> catToType Nothing (Cat "A")
-- A
-- >>> catToType Nothing (ListCat (Cat "A"))
-- [A]
-- >>> catToType Nothing (TokenCat "Ident")
-- Ident
--
-- Note that there is no haskell type for coerced categories: they should be
-- normalized
-- >>> catToType Nothing (CoercCat "Expr" 2)
-- Expr
--
-- If a type parameter is given it is added to the type name:
-- >>> catToType (Just "a") (Cat "A")
-- (A a)
--
-- >>> catToType (Just "a") (ListCat (Cat "A"))
-- [A a]
--
-- but not added to Token categories:
-- >>> catToType (Just "a") (TokenCat "Integer")
-- Integer
--
-- >>> catToType (Just "a") (ListCat (TokenCat "Integer"))
-- [Integer]
--
-- >>> catToType Nothing (ListCat (CoercCat "Exp" 2))
-- [Exp]
--
-- >>> catToType (Just "()") (ListCat (CoercCat "Exp" 2))
-- [Exp ()]
catToType :: Maybe Doc -> Cat -> Doc
catToType param cat = maybeParens $ catToType' param cat
  where
    maybeParens = case (param,cat) of
        (Just _, Cat _) -> parens
        _ -> id
    catToType' _ InternalCat = error "Can't create a haskell type for internal category"
    catToType' Nothing c = text $ show $ normCat c
    catToType' (Just p) (Cat c) = text c <+> p
    catToType' (Just p) (CoercCat c _) = text c <+> p
    catToType' (Just _) (TokenCat c) = text c
    catToType' (Just p) (ListCat c) = lbrack <> catToType' (Just p) c <> rbrack


-- | gives you a list of variables usable for pattern matching.
-- Ex: if you have the rule Aba. S ::= A B A ; with the generated data type
--   data S = Aba A B A
-- Given the lit of categories in the RHS of the rule (A B A), we generate the
-- list [a1,b,a2] to be used in a pattern matching like
-- case s of
--     Aba a1 b a2 -> ...
--     ...
--
-- >>> catvars [Cat "A", Cat "B", Cat "A"]
-- [a1,b,a2]
--
-- It should avoid reserved words
-- >>> catvars [Cat "IF", Cat "Case", Cat "Type", Cat "If"]
-- [if_1,case_,type_,if_2]
--
-- It uses an -s to mark lists:
-- >>> catvars [Cat "A", ListCat (Cat "A"), ListCat (ListCat (Cat "A"))]
-- [a,as_,ass]
catvars :: [Cat] -> [Doc]
catvars = map text . mkNames hsReservedWords LowerCase . map var
  where
    var (ListCat c) = var c ++ "s"
    var xs          = show xs

