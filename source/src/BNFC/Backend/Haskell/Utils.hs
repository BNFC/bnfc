{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module BNFC.Backend.Haskell.Utils
  ( comment
  , posType, posConstr, noPosConstr
  , hasPositionClass, hasPositionMethod
  , noWarnUnusedMatches
  , parserName
  , hsReservedWords, avoidReservedWords, mkDefName
  , typeToHaskell, typeToHaskell'
  , catToType
  , catToVar, catvars
  , tokenTextImport, tokenTextType
  , tokenTextPack, tokenTextPackParens, tokenTextUnpack
  ) where

import Data.Char
import Data.String (IsString)

import BNFC.PrettyPrint
import qualified BNFC.PrettyPrint as P

import BNFC.CF      (Cat(..), catToStr, identCat, baseTokenCatNames, Base, Type(FunT), IsFun(..))
import BNFC.Options (TokenText(..))
import BNFC.Utils   (mkNames, NameStyle(..))

-- | Haskell line comments.

comment :: String -> String
comment = ("-- " ++)

-- * GHC pragmas

noWarnUnusedMatches :: IsString a => a
noWarnUnusedMatches =
  "{-# OPTIONS_GHC -fno-warn-unused-matches #-}"
  -- ALT: only from GHC 8
  -- "{-# OPTIONS_GHC -Wno-unused-matches #-}"

-- * Names for position data type.

posType, posConstr, noPosConstr :: IsString a => a

posType     = "BNFC'Position"
posConstr   = "BNFC'Position"
noPosConstr = "BNFC'NoPosition"

-- * The @HasPosition@ class for position-carrying abstract syntax.

hasPositionClass, hasPositionMethod :: IsString a => a

hasPositionClass  = "HasPosition"
hasPositionMethod = "hasPosition"

-- * Parameterization by 'TokenText'.

tokenTextImport :: TokenText -> [String]
tokenTextImport = \case
  StringToken     -> []
  ByteStringToken -> [ "import qualified Data.ByteString.Char8 as BS" ]
  TextToken       -> [ "import qualified Data.Text" ]

tokenTextType :: TokenText -> String
tokenTextType = \case
  StringToken     -> "String"
  ByteStringToken -> "BS.ByteString"
  TextToken       -> "Data.Text.Text"

tokenTextPack :: TokenText -> String -> String
tokenTextPack = \case
  StringToken     -> id
  ByteStringToken -> ("BS.pack " ++)
  TextToken       -> ("Data.Text.pack " ++)

tokenTextPackParens :: TokenText -> String -> String
tokenTextPackParens = \case
  StringToken     -> id
  ByteStringToken -> parens . ("BS.pack " ++)
  TextToken       -> parens . ("Data.Text.pack " ++)
  where
  parens :: String -> String
  parens s = "(" ++ s ++ ")"

tokenTextUnpack :: TokenText -> String -> String
tokenTextUnpack t s = case t of
  StringToken     -> s
  ByteStringToken -> "(BS.unpack " ++ s ++ ")"
  TextToken       -> "(Data.Text.unpack " ++ s ++ ")"

-- * Other Utililites

-- | Create a valid parser function name for a given category.
--
-- >>> parserName (Cat "Abcd")
-- pAbcd
--
-- >>> parserName (ListCat (Cat "Xyz"))
-- pListXyz
--
parserName :: Cat -> Doc
parserName = ("p" P.<>) . text . identCat

-- | Haskell's reserved words.
--
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
    , "family"
    , "forall"
    , "foreign"
    , "hiding"
    , "if"
    , "import"
    , "in"
    , "infix"
    , "infixl"
    , "infixr"
    , "instance"
    , "let"
    , "mdo"
    , "module"
    , "newtype"
    , "of"
    , "pattern"
    , "proc"
    , "qualified"
    , "rec"
    , "then"
    , "type"
    , "where"
    ]

-- | Avoid Haskell keywords plus additional reserved words.

avoidReservedWords :: [String] -> String -> String
avoidReservedWords additionalReserved x
  | x `elem` reserved = x ++ "'"
  | otherwise         = x
  where
  reserved = additionalReserved ++ hsReservedWords

-- | Modifier to avoid clashes in definition.
mkDefName :: IsFun f => f -> String
mkDefName = avoidReservedWords [] . funName

-- | Render a category from the grammar to a Haskell type.
--
-- >>> catToType id empty (Cat "A")
-- A
-- >>> catToType id empty (ListCat (Cat "A"))
-- [A]
-- >>> catToType ("Foo." P.<>) empty (TokenCat "Ident")
-- Foo.Ident
--
-- Note that there is no haskell type for coerced categories: they should be normalized:
-- >>> catToType id empty (CoercCat "Expr" 2)
-- Expr
--
-- If a type parameter is given it is added to the type name:
-- >>> catToType id (text "a") (Cat "A")
-- (A a)
--
-- >>> catToType id (text "a") (ListCat (Cat "A"))
-- [A a]
--
-- but not added to Token categories:
-- >>> catToType ("Foo." P.<>) (text "a") (TokenCat "Integer")
-- Integer
--
-- >>> catToType id (text "a") (ListCat (TokenCat "Integer"))
-- [Integer]
--
-- >>> catToType id empty (ListCat (CoercCat "Exp" 2))
-- [Exp]
--
-- >>> catToType ("Foo." P.<>) (text "()") (ListCat (CoercCat "Exp" 2))
-- [Foo.Exp ()]
--
catToType :: (Doc -> Doc) -> Doc -> Cat -> Doc
catToType qualify param cat = parensIf isApp $ loop cat
  where
    isApp = case cat of
        Cat _ -> not $ isEmpty param
        _ -> False
    loop = \case
      ListCat c     -> brackets $ loop c
      Cat c         -> qualify (text c) <+> param  -- note: <+> goes away if param==empty
      CoercCat c _  -> qualify (text c) <+> param
      TokenCat c
        | c `elem` baseTokenCatNames
                    -> text c
        | otherwise -> qualify (text c)

-- | Convert a base type to Haskell syntax.
baseTypeToHaskell :: Base -> String
baseTypeToHaskell = show

-- | Convert a function type to Haskell syntax in curried form.
typeToHaskell :: Type -> String
typeToHaskell = typeToHaskell' "->"

typeToHaskell' :: String -> Type -> String
typeToHaskell' arr (FunT ts t) =
  foldr f (baseTypeToHaskell t) $ map baseTypeToHaskell ts
  where f a b = unwords [a, arr, b]

-- | Make a variable name for a category.
catToVar :: [String] -> Cat -> String
catToVar rs = avoidReservedWords rs . var
  where
  var (ListCat cat)   = var cat ++ "s"
  var (Cat "Ident")   = "x"
  var (Cat "Integer") = "n"
  var (Cat "String")  = "str"
  var (Cat "Char")    = "c"
  var (Cat "Double")  = "d"
  var xs              = map toLower $ catToStr xs

-- | Gives a list of variables usable for pattern matching.
--
-- Example: Given the rule @Aba. S ::= A B A ;@ with the generated data type
-- @
--   data S = Aba A B A
-- @
-- from the list of categories on the RHS of the rule [A,B,A], we generate the
-- list [a1,b,a2] to be used in a pattern matching like
-- @
--   case s of
--     Aba a1 b a2 -> ...
--     ...
-- @
--
-- >>> catvars [] [Cat "A", Cat "B", Cat "A"]
-- [a1,b,a2]
--
-- It should avoid reserved words:
-- >>> catvars ["foo"] [Cat "Foo", Cat "IF", Cat "Case", Cat "Type", Cat "If"]
-- [foo_,if_1,case_,type_,if_2]
--
-- It uses a suffix -s to mark lists:
-- >>> catvars [] [Cat "A", ListCat (Cat "A"), ListCat (ListCat (Cat "A"))]
-- [a,as_,ass]
--
catvars :: [String] -> [Cat] -> [Doc]
catvars rs = map text . mkNames (rs ++ hsReservedWords) LowerCase . map var
  where
    var (ListCat c) = var c ++ "s"
    var c           = catToStr c
