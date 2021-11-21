{-
    BNF Converter: Happy Generator
    Copyright (C) 2004  Author:  Markus Forsberg, Aarne Ranta

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module BNFC.Backend.Haskell.CFtoHappy (cf2Happy, convert) where

import Prelude hiding ((<>))

import Data.Foldable (toList)
import Data.List (intersperse)

import BNFC.CF
import BNFC.Backend.Common.StrUtils (escapeChars)
import BNFC.Backend.Haskell.Utils
import BNFC.Options (HappyMode(..), TokenText(..))
import BNFC.PrettyPrint
import BNFC.Utils

-- Type declarations

type Rules       = [(NonTerminal,[(Pattern,Action)])]
type Pattern     = String
type Action      = String
type MetaVar     = String

-- default naming

tokenName :: String
tokenName = "Token"

-- | Generate a happy parser file from a grammar.

cf2Happy
  :: ModuleName -- ^ This module's name.
  -> ModuleName -- ^ Abstract syntax module name.
  -> ModuleName -- ^ Lexer module name.
  -> HappyMode  -- ^ Happy mode.
  -> TokenText  -- ^ Use @ByteString@ or @Text@?
  -> Bool       -- ^ AST is a functor?
  -> CF         -- ^ Grammar.
  -> String     -- ^ Generated code.
cf2Happy name absName lexName mode tokenText functor cf = unlines
  [ header name absName lexName tokenText eps
  , render $ declarations mode functor eps
  , render $ tokens cf functor
  , delimiter
  , specialRules absName functor tokenText cf
  , render $ prRules absName functor (rulesForHappy absName functor cf)
  , ""
  , footer absName tokenText functor eps cf
  ]
  where
  eps = toList $ allEntryPoints cf

-- | Construct the header.
header :: ModuleName -> ModuleName -> ModuleName -> TokenText -> [Cat] -> String
header modName absName lexName tokenText eps = unlines $ concat
  [ [ "-- Parser definition for use with Happy"
    , "{"
    , "{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}"
    , "{-# LANGUAGE PatternSynonyms #-}"
    , ""
    , "module " ++ modName
    , "  ( happyError"
    , "  , myLexer"
    ]
  , map (("  , " ++) . render . parserName) eps
  , [ "  ) where"
    , ""
    , "import Prelude"
    , ""
    , "import qualified " ++ absName
    , "import " ++ lexName
    ]
  , tokenTextImport tokenText
  , [ ""
    , "}"
    ]
  ]

-- | The declarations of a happy file.
-- >>> declarations Standard False [Cat "A", Cat "B", ListCat (Cat "B")]
-- %name pA A
-- %name pB B
-- %name pListB ListB
-- -- no lexer declaration
-- %monad { Err } { (>>=) } { return }
-- %tokentype {Token}
--
-- >>> declarations Standard True [Cat "A", Cat "B", ListCat (Cat "B")]
-- %name pA_internal A
-- %name pB_internal B
-- %name pListB_internal ListB
-- -- no lexer declaration
-- %monad { Err } { (>>=) } { return }
-- %tokentype {Token}
declarations :: HappyMode -> Bool -> [Cat] -> Doc
declarations mode functor ns = vcat
    [ vcat $ map generateP ns
    , case mode of
        Standard -> "-- no lexer declaration"
        GLR      -> "%lexer { myLexer } { Err _ }",
      "%monad { Err } { (>>=) } { return }",
      "%tokentype" <+> braces (text tokenName)
    ]
  where
  generateP n = "%name" <+> parserName n <> (if functor then "_internal" else "") <+> text (identCat n)

-- The useless delimiter symbol.
delimiter :: String
delimiter = "\n%%\n"

-- | Generate the list of tokens and their identifiers.
tokens :: CF -> Bool -> Doc
tokens cf functor
  -- Andreas, 2019-01-02: "%token" followed by nothing is a Happy parse error.
  -- Thus, if we have no tokens, do not output anything.
  | null ts   = empty
  | otherwise = "%token" $$ (nest 2 $ vcat $ map text $ table " " ts)
  where
    ts            = map prToken (cfTokens cf) ++ specialToks cf functor
    prToken (t,k) = [ render (convert t), "{ PT _ (TS _ " ++ show k ++ ")", "}" ]

-- Happy doesn't allow characters such as åäö to occur in the happy file. This
-- is however not a restriction, just a naming paradigm in the happy source file.
convert :: String -> Doc
convert = quotes . text . escapeChars

rulesForHappy :: ModuleName -> Bool -> CF -> Rules
rulesForHappy absM functor cf = for (ruleGroups cf) $ \ (cat, rules) ->
  (cat, map (constructRule absM functor) rules)

-- | For every non-terminal, we construct a set of rules. A rule is a sequence
-- of terminals and non-terminals, and an action to be performed.
--
-- >>> constructRule "Foo" False (npRule "EPlus" (Cat "Exp") [Left (Cat "Exp"), Right "+", Left (Cat "Exp")] Parsable)
-- ("Exp '+' Exp","Foo.EPlus $1 $3")
--
-- If we're using functors, it adds position value:
--
-- >>> constructRule "Foo" True (npRule "EPlus" (Cat "Exp") [Left (Cat "Exp"), Right "+", Left (Cat "Exp")] Parsable)
-- ("Exp '+' Exp","(fst $1, Foo.EPlus (fst $1) (snd $1) (snd $3))")
--
-- List constructors should not be prefixed by the abstract module name:
--
-- >>> constructRule "Foo" False (npRule "(:)" (ListCat (Cat "A")) [Left (Cat "A"), Right",", Left (ListCat (Cat "A"))] Parsable)
-- ("A ',' ListA","(:) $1 $3")
--
-- >>> constructRule "Foo" False (npRule "(:[])" (ListCat (Cat "A")) [Left (Cat "A")] Parsable)
-- ("A","(:[]) $1")
--
-- Coercion are much simpler:
--
-- >>> constructRule "Foo" True (npRule "_" (Cat "Exp") [Right "(", Left (Cat "Exp"), Right ")"] Parsable)
-- ("'(' Exp ')'","(uncurry Foo.BNFC'Position (tokenLineCol $1), (snd $2))")
--
constructRule :: IsFun f => String -> Bool -> Rul f -> (Pattern, Action)
constructRule absName functor (Rule fun0 _cat rhs Parsable) = (pat, action)
  where
    fun = funName fun0
    (pat, metavars) = generatePatterns functor rhs
    action
      | functor   = "(" ++ actionPos id ++ ", " ++ actionValue ++ ")"
      | otherwise = actionValue
    actionPos paren = case rhs of
      []          -> qualify noPosConstr
      (Left _:_)  -> paren "fst $1"
      (Right _:_) -> paren $ unwords [ "uncurry", qualify posConstr , "(tokenLineCol $1)" ]
    actionValue
      | isCoercion fun = unwords metavars
      | isNilCons  fun = unwords (qualify fun : metavars)
      | functor        = unwords (qualify fun : actionPos (\ x -> "(" ++ x ++ ")") : metavars)
      | otherwise      = unwords (qualify fun : metavars)
    qualify f
      | isConsFun f || isNilCons f = f
      | isDefinedRule f = absName ++ "." ++ mkDefName f
      | otherwise       = absName ++ "." ++ f
constructRule _ _ (Rule _ _ _ Internal) = undefined -- impossible


-- | Generate patterns and a set of metavariables (de Bruijn indices) indicating
--   where in the pattern the non-terminal are locate.
--
-- >>> generatePatterns False [ Left (Cat "Exp"), Right "+", Left (Cat "Exp") ]
-- ("Exp '+' Exp",["$1","$3"])
--
-- >>> generatePatterns True [ Left (Cat "Exp"), Right "+", Left (Cat "Exp") ]
-- ("Exp '+' Exp",["(snd $1)","(snd $3)"])
--
generatePatterns :: Bool -> SentForm -> (Pattern, [MetaVar])
generatePatterns _       []  = ("{- empty -}", [])
generatePatterns functor its =
  ( unwords $ for its $ either {-non-term:-} identCat {-term:-} (render . convert)
  , [ if functor then "(snd $" ++ show i ++ ")" else ('$' : show i) | (i, Left{}) <- zip [1 :: Int ..] its ]
  )

-- We have now constructed the patterns and actions,
-- so the only thing left is to merge them into one string.

-- |
-- >>> prRules "Foo" False [(Cat "Expr", [("Integer", "Foo.EInt $1"), ("Expr '+' Expr", "Foo.EPlus $1 $3")])]
-- Expr :: { Foo.Expr }
-- Expr : Integer { Foo.EInt $1 } | Expr '+' Expr { Foo.EPlus $1 $3 }
--
-- if there's a lot of cases, print on several lines:
-- >>> prRules "" False [(Cat "Expr", [("Abcd", "Action"), ("P2", "A2"), ("P3", "A3"), ("P4", "A4"), ("P5","A5"), ("P6", "A6"), ("P7", "A7"), ("P8", "A8"), ("P9","A9")])]
-- Expr :: { Expr }
-- Expr
--   : Abcd { Action }
--   | P2 { A2 }
--   | P3 { A3 }
--   | P4 { A4 }
--   | P5 { A5 }
--   | P6 { A6 }
--   | P7 { A7 }
--   | P8 { A8 }
--   | P9 { A9 }
--
-- >>> prRules "" False [(Cat "Internal", [])] -- nt has only internal use
-- <BLANKLINE>
--
-- The functor case:
-- >>> prRules "" True [(Cat "Expr", [("Integer", "EInt () $1"), ("Expr '+' Expr", "EPlus () $1 $3")])]
-- Expr :: { (BNFC'Position, Expr) }
-- Expr : Integer { EInt () $1 } | Expr '+' Expr { EPlus () $1 $3 }
--
-- A list with coercion: in the type signature we need to get rid of the
-- coercion.
--
-- >>> prRules "" True [(ListCat (CoercCat "Exp" 2), [("Exp2", "(:[]) $1"), ("Exp2 ',' ListExp2","(:) $1 $3")])]
-- ListExp2 :: { (BNFC'Position, [Exp]) }
-- ListExp2 : Exp2 { (:[]) $1 } | Exp2 ',' ListExp2 { (:) $1 $3 }
--
prRules :: ModuleName -> Bool -> Rules -> Doc
prRules absM functor = vsep . map prOne
  where
    prOne (_ , []      ) = empty -- nt has only internal use
    prOne (nt, (p,a):ls) = vcat
        [ hsep [ nt', "::", "{", if functor then functorType' nt else type' nt, "}" ]
        , hang nt' 2 $ sep (pr ":" (p, a) : map (pr "|") ls)
        ]
      where
        nt'          = text (identCat nt)
        pr pre (p,a) = hsep [pre, text p, "{", text a , "}"]
    type'            = catToType qualify empty
    functorType' nt  = hcat ["(", qualify posType, ", ", type' nt, ")"]
    qualify
      | null absM = id
      | otherwise = ((text absM <> ".") <>)

-- Finally, some haskell code.

footer :: ModuleName -> TokenText -> Bool -> [Cat] -> CF -> String
footer absName tokenText functor eps _cf = unlines $ concat
  [ [ "{"
    , ""
    , "type Err = Either String"
    , ""
    , "happyError :: [" ++ tokenName ++ "] -> Err a"
    , "happyError ts = Left $"
    , "  \"syntax error at \" ++ tokenPos ts ++ "
    , "  case ts of"
    , "    []      -> []"
    , "    [Err _] -> \" due to lexer error\""
    , unwords
      [ "    t:_     -> \" before `\" ++"
      , "(prToken t)"
      -- , tokenTextUnpack tokenText "(prToken t)"
      , "++ \"'\""
      ]
    , ""
    , "myLexer :: " ++ tokenTextType tokenText ++ " -> [" ++ tokenName ++ "]"
    , "myLexer = tokens"
    , ""
    ]
  , when functor
    [ "-- Entrypoints"
    , ""
    , render . vsep $ map mkParserFun eps
    ]
  , [ "}" ]
  ]
  where
    mkParserFun cat = vcat
      [ parserName cat <+> "::" <+> brackets (text tokenName) <+> "-> Err" <+> catToType qualify empty cat
      , parserName cat <+> "=" <+>  "fmap snd" <+> "." <+> parserName cat <> "_internal"
      ]
    qualify
      | null absName = id
      | otherwise    = ((text absName <> ".") <>)

-- | GF literals.
specialToks :: CF -> Bool -> [[String]]  -- ^ A table with three columns (last is "}").
specialToks cf functor = (`map` literals cf) $ \t -> case t of
  "Ident"   -> [ "L_Ident" , "{ PT _ (TV " ++ posn t ++ ")", "}" ]
  "String"  -> [ "L_quoted", "{ PT _ (TL " ++ posn t ++ ")", "}" ]
  "Integer" -> [ "L_integ ", "{ PT _ (TI " ++ posn t ++ ")", "}" ]
  "Double"  -> [ "L_doubl ", "{ PT _ (TD " ++ posn t ++ ")", "}" ]
  "Char"    -> [ "L_charac", "{ PT _ (TC " ++ posn t ++ ")", "}" ]
  own       -> [ "L_" ++ own,"{ PT _ (T_" ++ own ++ " " ++ posn own ++ ")", "}" ]
  where
    posn tokenCat = if isPositionCat cf tokenCat || functor then "_" else "$$"

specialRules :: ModuleName -> Bool -> TokenText -> CF -> String
specialRules absName functor tokenText cf = unlines . intersperse "" . (`map` literals cf) $ \t -> case t of
    -- "Ident"   -> "Ident   :: { Ident }"
    --         ++++ "Ident    : L_ident  { Ident $1 }"
    "String"  -> "String  :: { " ++ mkTypePart t ++ " }"
            ++++ "String   : L_quoted { " ++ mkBodyPart t ++ " }"
    "Integer" -> "Integer :: { " ++ mkTypePart t ++ " }"
            ++++ "Integer  : L_integ  { " ++ mkBodyPart t ++ " }"
    "Double"  -> "Double  :: { " ++ mkTypePart t ++ " }"
            ++++ "Double   : L_doubl  { " ++ mkBodyPart t ++ " }"
    "Char"    -> "Char    :: { " ++ mkTypePart t ++ " }"
            ++++ "Char     : L_charac { " ++ mkBodyPart t ++ " }"
    own       -> own ++ " :: { " ++ mkTypePart (qualify own) ++ " }"
            ++++ own ++ "  : L_" ++ own ++ " { " ++ mkBodyPart t ++ " }"
  where
    mkTypePart tokenCat = if functor then concat [ "(", qualify posType, ", ", tokenCat, ")" ] else tokenCat
    mkBodyPart tokenCat
      | functor   = "(" ++ unwords ["uncurry", qualify posConstr, "(tokenLineCol $1)"] ++ ", " ++ mkValPart tokenCat ++ ")"
      | otherwise = mkValPart tokenCat
    mkValPart tokenCat =
      case tokenCat of
        "String"  -> if functor then stringUnpack "((\\(PT _ (TL s)) -> s) $1)"
                                else stringUnpack "$1"                                 -- String never has pos
        "Integer" -> if functor then "(read " ++ stringUnpack "(tokenText $1)" ++ ") :: Integer"
                                else "(read " ++ stringUnpack "$1" ++ ") :: Integer" -- Integer never has pos
        "Double"  -> if functor then "(read " ++ stringUnpack "(tokenText $1)" ++ ") :: Double"
                                else "(read " ++ stringUnpack "$1" ++ ") :: Double"  -- Double never has pos
        "Char"    -> if functor then "(read " ++ stringUnpack "(tokenText $1)" ++ ") :: Char"
                                else "(read " ++ stringUnpack "$1" ++ ") :: Char"    -- Char never has pos
        own       ->
          case functor of
            False ->
              case isPositionCat cf tokenCat of
                False -> qualify own ++ " $1"
                True  -> qualify own ++ " (mkPosToken $1)"
            True  ->
              case isPositionCat cf tokenCat of
                False -> qualify own ++ " (tokenText $1)"
                True  -> qualify own ++ " (mkPosToken $1)"
    stringUnpack = tokenTextUnpack tokenText
    qualify
      | null absName = id
      | otherwise    = ((absName ++ ".") ++)
