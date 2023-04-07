{-
    BNF Converter: ocamlyacc Generator
    Copyright (C) 2005  Author:  Kristofer Johannisson

-}

-- based on BNFC Haskell backend

{-# LANGUAGE LambdaCase #-}

module BNFC.Backend.OCaml.CFtoOCamlYacc
       (
       cf2ocamlyacc, terminal, epName
       )
        where

import Data.Char
import Data.Foldable ( toList )
import Data.List     ( intercalate )

import BNFC.CF
import BNFC.Options  ( OCamlParser(..) )
import BNFC.Utils    ( (+++), mapHead, table )
import BNFC.Backend.Common
import BNFC.Backend.OCaml.OCamlUtil

-- Type declarations

type Pattern     = String
type Action      = String
type MetaVar     = String

-- The main function, that given a CF
-- generates a ocamlyacc module.
cf2ocamlyacc :: OCamlParser -> String -> CF -> String
cf2ocamlyacc ocamlParser absName cf = unlines
  [ header ocamlParser absName
  , declarations absName cf
  , "%%"
  , ""
  , rules ocamlParser cf
  ]


header :: OCamlParser -> String -> String
header ocamlParser absName = unlines
  [ unwords [ "/* Parser definition for use with", ocamlParserName ocamlParser, "*/" ]
  , ""
  , "%{"
  , "open " ++ absName
  , "open Lexing"
  , "%}"
  ]

declarations :: String -> CF -> String
declarations absName cf =
  unlines $ intercalate [""]
    [ tokens (unicodeAndSymbols cf) (asciiKeywords cf)
    , specialTokens cf
    , entryPoints absName cf
    , map (catTyping . fst)      $ ruleGroups cf
    , map (catTyping . TokenCat) $ literals cf
    ]
  where
    catTyping c = typing absName c (nonterminal c)

-- | Declare keyword and symbol tokens.

tokens :: [String] -> [String] -> [String]
tokens symbols reswords =
  concat
    [ [ unwords $ "%token" : map ("KW_" ++) reswords | hasReserved ]
    , [ "" | hasReserved ]
    , (`map` zip symbols [1::Int ..]) $ \ (s, n) ->
        "%token SYMB" ++ show n +++ "/*" +++ s +++ "*/"
    ]
  where
  hasReserved = not $ null reswords

-- | map a CF terminal into a ocamlyacc token
terminal :: CF -> String -> String
terminal cf = \ s ->
    -- Use a lambda here to make sure that kws is computed before the
    -- second argument is applied.
    -- The GHC manual says that let-floating is not consistently applied
    -- so just writing @terminal cf s = ...@ could result in computing
    -- kws for every @s@ anew.
    if s `elem` kws then "KW_" ++ s
    else case lookup s (zip (unicodeAndSymbols cf) [1::Int ..]) of
      Just i -> "SYMB" ++ show i
      Nothing -> error $ "CFtoOCamlYacc: terminal " ++ show s ++ " not defined in CF."
  where
  kws = asciiKeywords cf

-- | map a CF nonterminal into a ocamlyacc symbol
nonterminal :: Cat -> String
nonterminal c = map spaceToUnderscore (fixType c)
    where spaceToUnderscore ' ' = '_'
          spaceToUnderscore x = x

specialTokens :: CF -> [String]
specialTokens cf = concat $
  [ [ "%token TOK_EOF" ]
  , table " " [ prToken (ty n)      n | n                 <- specialCatsP  ]
  , table " " [ prToken (posTy pos) n | TokenReg n0 pos _ <- cfgPragmas cf, let n = wpThing n0 ]
  ]
  where
  prToken t n = [ "%token" +++ t, "TOK_" ++ n ]
  ty = \case
    "Ident"   -> "<string>"
    "String"  -> "<string>"
    "Integer" -> "<int>"
    "Double"  -> "<float>"
    "Char"    -> "<char>"
    _ -> undefined
  posTy = \case
    True  -> "<(int * int) * string>"
    False -> "<string>"

entryPoints :: String -> CF -> [String]
entryPoints absName cf =
  concat
    [ [ unwords $ "%start" : map epName eps ]
    , map (\ c -> typing absName c (epName c)) eps
    ]
  where
    eps = toList $ allEntryPoints cf

typing :: String -> Cat -> String -> String
typing absName c s = "%type" +++ "<" ++ qualify (normCat c) ++ ">" +++ s
    where
          qualify c = if c `elem` [ TokenCat "Integer", TokenCat "Double", TokenCat "Char",
                                    TokenCat "String", ListCat (TokenCat "Integer"),
                                    ListCat (TokenCat "Double"),
                                    ListCat (TokenCat "Char"),
                                    ListCat (TokenCat "String") ]
                      then fixType c
                      else absName ++ "." ++ fixType c

epName :: Cat -> String
epName c = "p" ++ mapHead toUpper (nonterminal c)

entryPointRules :: OCamlParser -> CF -> [String]
entryPointRules ocamlParser cf =
  map (unlines . mkRule) $ toList $ allEntryPoints cf
  where
  mkRule :: Cat -> [String]
  mkRule = case ocamlParser of
    Menhir    -> \ cat ->
      [ epRule cat ++ ";" ]
    OCamlYacc -> \ cat ->
      [ epRule cat
          -- Andreas, 2022-02-10, issue 414:
          -- We keep the 'error' token rule, throwing BNFC_Util.Parse_error,
          -- for API stability.
          -- It would be more uniform with the Menhir backend to just drop this rule
          -- and let the user catch the Parsing.Parse_error exception.
      , "  /* Delete this error clause to get a Parsing.Parse_error exception instead: */"
      , ocamlYaccErrorCase
      , "  ;"
      ]
  epRule :: Cat -> String
  epRule cat = epName cat ++ " : " ++ nonterminal cat ++ " TOK_EOF { $1 }"

ocamlYaccErrorCase :: String
ocamlYaccErrorCase = concat
  [ "  | error { raise (BNFC_Util.Parse_error ("
  , "Parsing.symbol_start_pos ()"
  , ", "
  , "Parsing.symbol_end_pos ()"
  , ")) }"
  ]

rules :: OCamlParser -> CF -> String
rules ocamlParser cf = unlines $ concat
    [ entryPointRules ocamlParser cf
    , map (prOne . mkOne) $ ruleGroups cf
    , specialRules cf
    ]
  where
    mkOne (cat,rules) = (cat, constructRule (terminal cf) rules cat)
    prOne (_  , []  ) = [] -- nt has only internal use
    prOne (cat, l:ls) = unlines $ concat
        [ [ unwords [ nt, ":", rule l ] ]
        , map (("  | " ++) . rule) ls
        , [ "  ;" ]
        ]
      where
        rule (p,a) = unwords [ p, "{", a , "}" ]
        nt = nonterminal cat


-- For every non-terminal, we construct a set of rules. A rule is a sequence of
-- terminals and non-terminals, and an action to be performed
constructRule :: (String -> String) -> [Rule] -> NonTerminal -> [(Pattern,Action)]
constructRule terminal rules nt =
  [ (p, generateAction nt (funRule r) m)
  | r <- rules
  , let (p, m) = generatePatterns terminal r
  ]



-- Generates a string containing the semantic action.
-- An action can for example be: Sum $1 $2, that is, construct an AST
-- with the constructor Sum applied to the two metavariables $1 and $2.
generateAction :: IsFun a => NonTerminal -> a -> [MetaVar] -> Action
generateAction _ f ms = (if isCoercion f then "" else f') +++ mkTuple ms
    where
    f' = case funName f of -- ocaml cons is somehow not a standard infix oper, right?
           "(:[])" -> "(fun x -> [x])"
           "(:)"   -> "(fun (x,xs) -> x::xs)"
           x       -> sanitizeOcaml x


generatePatterns :: (String -> String) -> Rule -> (Pattern,[MetaVar])
generatePatterns terminal r = case rhsRule r of
  []  -> ("/* empty */",[])
  its -> (unwords (map mkIt its), metas its)
 where
   mkIt i = case i of
     Left c -> nonterminal c
     Right s -> terminal s
   metas its = [ '$': show i | (i, Left _c) <- zip [1 ::Int ..] its ]

specialRules :: CF -> [String]
specialRules cf = (`map` literals cf) $ \case
  "Ident"   -> "ident : TOK_Ident  { Ident $1 };"
  "String"  -> "string : TOK_String { $1 };"
  "Integer" -> "int :  TOK_Integer  { $1 };"
  "Double"  -> "float : TOK_Double  { $1 };"
  "Char"    -> "char : TOK_Char { $1 };"
  own       -> concat $
    [ fixType (TokenCat own), " : TOK_", own, " { ", own, " (",  posn, "$1)};" ]
    where -- ignore position categories for now
    posn = "" -- if isPositionCat cf own then "mkPosToken " else ""
