{-
    BNF Converter: Abstract syntax Generator
    Copyright (C) 2004  Author:  Markus Forsberg

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module BNFC.Backend.Haskell.CFtoAbstract
  ( cf2Abstract
  , DefCfg(..), definedRules', definedRules
  ) where

import Prelude hiding ((<>))
import Data.Either (isRight)
import Data.Maybe
import qualified Data.List as List

import BNFC.CF
import BNFC.Options               ( SharedOptions(..), TokenText(..) )
import BNFC.PrettyPrint
import BNFC.Utils                 ( when, applyWhen )

import BNFC.Backend.Haskell.Utils
  ( avoidReservedWords, catToType, mkDefName
  , tokenTextImport, tokenTextType, typeToHaskell'
  , posType, posConstr, noPosConstr
  , hasPositionClass, hasPositionMethod
  )

-- | Create a Haskell module containing data type definitions for the abstract syntax.

cf2Abstract
  :: SharedOptions
  -> String    -- ^ Module name.
  -> CF        -- ^ Grammar.
  -> Doc
-- tokenText :: TokenText -- ^ Use @ByteString@ or @Text@ instead of @String@?
-- generic   :: Bool      -- ^ Derive @Data@, Generic@, @Typeable@?
-- functor   :: Bool      -- ^ Make the tree a functor?
cf2Abstract Options{ lang, tokenText, generic, functor } name cf = vsep . concat $
    [ []

    -- Modules header
    , [ vcat . concat $
        [ [ "{-# LANGUAGE DeriveDataTypeable #-}"         | gen ]
        , [ "{-# LANGUAGE DeriveGeneric #-}"              | gen ]
        , [ "{-# LANGUAGE DeriveTraversable #-}"          | fun ]
        , [ "{-# LANGUAGE FlexibleInstances #-}"          | fun ]
        , [ "{-# LANGUAGE GeneralizedNewtypeDeriving #-}" | hasIdentLikeNoPos ] -- for IsString
        , [ "{-# LANGUAGE LambdaCase #-}"                 | fun ]
        , [ "{-# LANGUAGE PatternSynonyms #-}"            | defPosition ]
        , [ "{-# LANGUAGE OverloadedStrings #-}"          | not (null definitions), tokenText /= StringToken ]
        ]
      ]
    , [ "-- | The abstract syntax of language" <+> text lang <> "." ]
    , [ hsep [ "module", text name, "where" ] ]

    -- Imports
    , [ vcat . concat $
        [ [ text $ "import Prelude (" ++ List.intercalate ", " typeImports ++ ")"
            | not $ null typeImports ]
        , [ prettyList 2 "import qualified Prelude as C" "(" ")" "," $ qualifiedPreludeImports
            | not $ null qualifiedPreludeImports ]
        , [ "import qualified Data.String"
            | hasIdentLikeNoPos ] -- for IsString
        ]
      ]
    , [ vcat . concat $
        [ when hasTextualToks $ map text $ tokenTextImport tokenText
        , [ "import qualified Data.Data    as C (Data, Typeable)" | gen ]
        , [ "import qualified GHC.Generics as C (Generic)"        | gen ]
        ]
      ]

    -- AST types
    , map (prData functor (derivingClasses functor)) datas

    -- Smart constructors
    , definitions

    -- Token definition types
    , (`map` specialCats cf) $ \ c ->
        let hasPos = isPositionCat cf c
        in  prSpecialData tokenText hasPos (derivingClassesTokenType hasPos) c

    -- BNFC'Position type
      -- We generate these synonyms for position info when --functor,
      -- regardless whether it is used in the abstract syntax.
      -- It may be used in the parser.
    , [ vcat
        [ "-- | Start position (line, column) of something."
        , ""
        , "type" <+> posType <+> "=" <+> "C.Maybe (C.Int, C.Int)"
        , ""
        , "pattern" <+> noPosConstr <+> "::" <+> posType
        , "pattern" <+> noPosConstr <+> "=" <+> "C.Nothing"
        , ""
        , "pattern" <+> posConstr <+> ":: C.Int -> C.Int ->" <+> posType
        , "pattern" <+> posConstr <+> "line col =" <+> "C.Just (line, col)"
        ]
      | defPosition
      ]

    -- HasPosition class
    , [ vcat
        [ "-- | Get the start position of something."
        , ""
        , "class" <+> hasPositionClass <+> "a where"
        , nest 2 $ hasPositionMethod <+> ":: a ->" <+> posType
        ]
      | hasPosition
      ]

    , when functor $ map instanceHasPositionData datas

    , map instanceHasPositionTokenType positionCats

    , [ "" ] -- ensure final newline
    ]
  where
    definitions  = definedRules functor cf

    datas        = cf2data cf
    positionCats = filter (isPositionCat cf) $ specialCats cf

    hasIdentLikeNoPos = hasIdentLikeTokens cf
    hasTextualToks    = hasTextualTokens cf
    hasPosToks   = hasPositionTokens cf
    hasData      = not (null datas)
    -- @defPosition@: should the @BNCF'Position@ type be defined?
    defPosition  = hasPosToks || functor
    -- @hasPosition@: should the @HasPosition@ class be defined?
    hasPosition  = hasPosToks || fun
    gen   = generic && hasData
    fun   = functor && hasData

    stdClasses = [ "Eq", "Ord", "Show", "Read" ]
    funClasses = [ "Functor", "Foldable", "Traversable" ]
    genClasses = [ "Data", "Typeable", "Generic" ]
    derivingClasses functor = map ("C." ++) $ concat
      [ stdClasses
      , when functor funClasses
      , when generic genClasses
      ]
    derivingClassesTokenType hasPos = concat
      [ derivingClasses False
      , [ "Data.String.IsString" | not hasPos ]
      ]
    -- import Prelude (Char, Double, Integer, String)
    typeImports =
      filter (\ s -> hasData        && s `elem` cfgLiterals cf
                  || hasTextualToks && tokenText == StringToken && s == "String")
        baseTokenCatNames
    qualifiedPreludeImports = concat
      [ [ text $ List.intercalate ", " stdClasses | hasTextualToks || hasData ]
      , [ text $ List.intercalate ", " funClasses | fun ]
      , [ text $ "Int, Maybe(..)" | defPosition ]
      ]

-- |
--
-- >>> prData False ["Eq", "Ord", "Show", "Read"] (Cat "C", [("C1", [Cat "C"]), ("CIdent", [Cat "Ident"])])
-- data C = C1 C | CIdent Ident
--   deriving (Eq, Ord, Show, Read)
--
-- Note that the layout adapts if it does not fit in one line:
-- >>> prData False ["Show"] (Cat "C", [("CAbracadabra",[]),("CEbrecedebre",[]),("CIbricidibri",[]),("CObrocodobro",[]),("CUbrucudubru",[])])
-- data C
--     = CAbracadabra
--     | CEbrecedebre
--     | CIbricidibri
--     | CObrocodobro
--     | CUbrucudubru
--   deriving (Show)
--
-- If the first argument is @True@, generate a functor:
-- >>> prData True ["Show", "Functor"] (Cat "C", [("C1", [Cat "C"]), ("CIdent", [TokenCat "Ident"])])
-- type C = C' BNFC'Position
-- data C' a = C1 a (C' a) | CIdent a Ident
--   deriving (Show, Functor)
--
-- The case for lists:
-- >>> prData True ["Show", "Functor"] (Cat "ExpList", [("Exps", [ListCat (Cat "Exp")])])
-- type ExpList = ExpList' BNFC'Position
-- data ExpList' a = Exps a [Exp' a]
--   deriving (Show, Functor)
--
prData :: Bool -> [String] -> Data -> Doc
prData functor derivingClasses (cat,rules) = vcat $ concat
  [ [ hsep [ "type", unprimedType, "=", primedType, posType ] | functor ]
  , [ hang ("data" <+> dataType) 4 $
        constructors rules ]
  , [ nest 2 $ deriving_ derivingClasses ]
  ]
  where
    prRule (fun, cats) = hsep $ concat [ [text fun], ["a" | functor], map prArg cats ]
    unprimedType       = pretty cat
    primedType         = prime unprimedType
    prime              = (<> "'")
    dataType | functor = primedType <+> "a"
             |otherwise= unprimedType
    prArg c
      | functor && (not .isRight . baseCat) c
                       = catToType prime "a" c
      | otherwise      = catToType id empty c
    constructors []    = empty
    constructors (h:t) = sep $ ["=" <+> prRule h] ++ map (("|" <+>) . prRule) t

-- | Generate @instance HasPosition@ for a data type.
--
-- >>> instanceHasPositionData (Cat "C", [("C1", [Cat "C"]), ("CIdent", [Cat "Ident"])])
-- instance HasPosition C where
--   hasPosition = \case
--     C1 p _ -> p
--     CIdent p _ -> p
--
-- >>> instanceHasPositionData (Cat "ExpList", [("Exps", [ListCat (Cat "Exp")])])
-- instance HasPosition ExpList where
--   hasPosition = \case
--     Exps p _ -> p

instanceHasPositionData :: Data -> Doc
instanceHasPositionData (cat, rules) = vcat . concat $
  [ [ "instance" <+> hasPositionClass <+> dat <+> "where" ]
  , [ nest 2 $ "hasPosition = \\case" ]
  , map (\ (c, args) -> nest 4 . hsep $ concat [ [text c, pos], "_" <$ args, ["->", pos] ]) rules
  ]
  where
  dat = text $ catToStr cat
  pos = "p"

-- | Generate a newtype declaration for Ident types
--
-- >>> prSpecialData StringToken False ["Show","Data.String.IsString"] catIdent
-- newtype Ident = Ident String
--   deriving (Show, Data.String.IsString)
--
-- >>> prSpecialData StringToken True ["Show"] catIdent
-- newtype Ident = Ident ((C.Int, C.Int), String)
--   deriving (Show)
--
-- >>> prSpecialData TextToken False ["Show"] catIdent
-- newtype Ident = Ident Data.Text.Text
--   deriving (Show)
--
-- >>> prSpecialData ByteStringToken False ["Show"] catIdent
-- newtype Ident = Ident BS.ByteString
--   deriving (Show)
--
-- >>> prSpecialData ByteStringToken True ["Show"] catIdent
-- newtype Ident = Ident ((C.Int, C.Int), BS.ByteString)
--   deriving (Show)
--
prSpecialData
  :: TokenText  -- ^ Format of token content.
  -> Bool       -- ^ If @True@, store the token position.
  -> [String]   -- ^ Derived classes.
  -> TokenCat   -- ^ Token category name.
  -> Doc
prSpecialData tokenText position classes cat = vcat
    [ hsep [ "newtype", text cat, "=", text cat, contentSpec ]
    , nest 2 $ deriving_ classes
    ]
  where
    contentSpec | position    = parens ( "(C.Int, C.Int), " <> stringType)
                | otherwise   = stringType
    stringType = text $ tokenTextType tokenText

-- | Generate 'deriving' clause
--
-- >>> deriving_ ["Show", "Read"]
-- deriving (Show, Read)
--
deriving_ :: [String] -> Doc
deriving_ cls = "deriving" <+> parens (hsep $ punctuate "," $ map text cls)

-- | Generate HasPosition instances for Ident types
--
-- >>> instanceHasPositionTokenType catIdent
-- instance HasPosition Ident where
--   hasPosition (Ident (p, _)) = C.Just p

instanceHasPositionTokenType :: TokenCat -> Doc
instanceHasPositionTokenType cat = vcat
  [ "instance" <+> hasPositionClass <+> t <+> "where"
  , nest 2 $ "hasPosition " <> parens (t <+> "(p, _)") <+> "= C.Just p"
  ]
  where
  t = text cat

-- | Parametrize 'definedRules' so that it can be used for Agda as well.

data DefCfg = DefCfg
  { sanitizeName :: String -> String
  , hasType      :: String
  , arrow        :: String
  , lambda       :: String
  , cons         :: String
  , convTok      :: String -> String
  , convLitInt   :: Exp -> Exp
  , polymorphism :: [Base] -> [Base]
  }

haskellDefCfg :: DefCfg
haskellDefCfg = DefCfg
  { sanitizeName = avoidReservedWords []
  , hasType      = "::"
  , arrow        = "->"
  , lambda       = "\\"
  , cons         = "(:)"
  , convTok      = id
  , convLitInt   = id
  , polymorphism = id
  }

-- | Generate Haskell code for the @define@d constructors.
definedRules :: Bool -> CF -> [Doc]
definedRules = definedRules' haskellDefCfg

-- | Generate Haskell/Agda code for the @define@d constructors.
definedRules' :: DefCfg -> Bool -> CF -> [Doc]
definedRules' DefCfg{..} functor cf = map mkDef $ definitions cf
  where
  mkDef (Define f args e _) = vcat $ concat
    [ [ text $ unwords [ fName, hasType, typeToHaskell' arrow $ typ $ wpThing t ]
      | t <- maybeToList $ sigLookup f cf
      ]
    , [ hsep $ concat
        [ map text [ fName, "=", lambda ]
        , map text $ addFunctorArg id $ map (sanitizeName . fst) args
        , [ text arrow, pretty $ sanitize e ]
        ]
      ]
    ]
    where
    fName = mkDefName f
    typ :: Type -> Type
    typ = applyWhen functor $ \ (FunT ts t) ->
            FunT (polymorphism $ BaseT "a" : map addParam ts) $ addParam t
    addParam :: Base -> Base
    addParam = fmap $ \ x -> if tokTyp x then x else x ++ "' a"
    tokTyp :: String -> Bool
    tokTyp = (`elem` literals cf)
    sanitize :: Exp -> Exp
    sanitize = \case
      App x t es
        | isConsFun x -> App cons t $ map sanitize es
        | isNilFun x  -> App x t $ map sanitize es
        | tokTyp x    -> App (convTok x) t $ map sanitize es
        | otherwise   -> App (sanitizeName x) t $ addFunctorArg (\ x -> App x dummyType []) $ map sanitize es
      Var x         -> Var $ sanitizeName x
      e@LitInt{}    -> convLitInt e
      e@LitDouble{} -> e
      e@LitChar{}   -> e
      e@LitString{} -> e
    -- Functor argument
    addFunctorArg :: (String -> a) -> [a] -> [a]
    addFunctorArg g = applyWhen functor (g "_a" :)
