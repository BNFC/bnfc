{-
    BNF Converter: fsyacc Generator
    Copyright (C) 2021  Author:  Grzegorz Dziadkiewicz

-}

-- based on BNFC OCaml backend


module BNFC.Backend.FSharp.CFtoFsYacc
       (
       cf2fsyacc, terminal, epName
       )
        where

import Data.Char
import Data.Foldable (toList)

import BNFC.CF
import BNFC.Utils ((+++))
import BNFC.Backend.Common
import BNFC.Backend.FSharp.FSharpUtil

-- Type declarations

type Pattern     = String
type Action      = String
type MetaVar     = String

-- The main function, that given a CF
-- generates a fsyacc module.
cf2fsyacc :: String -> String -> String -> CF -> String
cf2fsyacc _ absName lexName cf
 = unlines
    [header absName lexName cf,
    declarations absName cf,
    "%%",
    rules cf
    ]


header :: String -> String -> CF -> String
header absName _ _ = unlines
         ["/* Parser definition for use with fsyacc */",
          "%{",
          "open " ++ absName,
          "open FSharp.Text.Lexing",
          "",
          "%}"
         ]

declarations :: String -> CF -> String
declarations absName cf =
  unlines
    [ tokens (unicodeAndSymbols cf) (asciiKeywords cf)
    , specialTokens cf
    , entryPoints absName cf
    ]

-- | Declare keyword and symbol tokens.

tokens :: [String] -> [String] -> String
tokens symbols reswords =
  unlines $ concat
    [ [ unwords $ "%token" : map ("KW_" ++) reswords | hasReserved ]
    , [ "" | hasReserved ]
    , (`map` zip symbols [1..]) $ \ (s, n) ->
        "%token SYMB" ++ show n +++ "/*" +++ s +++ "*/"
    ]
  where
  hasReserved = not $ null reswords

-- | map a CF terminal into a fsyacc token
terminal :: CF -> String -> String
terminal cf = \ s ->
    -- Use a lambda here to make sure that kws is computed before the
    -- second argument is applied.
    -- The GHC manual says that let-floating is not consistently applied
    -- so just writing @terminal cf s = ...@ could result in computing
    -- kws for every @s@ anew.
    if s `elem` kws then "KW_" ++ s
    else case lookup s (zip (unicodeAndSymbols cf) [1..]) of
      Just i -> "SYMB" ++ show i
      Nothing -> error $ "CFtoFsYacc: terminal " ++ show s ++ " not defined in CF."
  where
  kws = asciiKeywords cf

specialTokens :: CF -> String
specialTokens cf = unlines $ concat $
  [ [ "%token TOK_EOF" ]
  , [ prToken (ty n)      n | n                 <- specialCatsP  ]
  , [ prToken (posTy pos) n | TokenReg n0 pos _ <- cfgPragmas cf, let n = wpThing n0 ]
  ]
  where
  prToken t n = "%token" +++ t +++ "TOK_" ++ n
  ty = \case
    "Ident"   -> "<string>"
    "String"  -> "<string>"
    "Integer" -> "<int>"
    "Double"  -> "<float>"
    "Char"    -> "<char>"
  posTy = \case
    True  -> "<(int * int) * string>"
    False -> "<string>"


entryPoints :: String -> CF -> String
entryPoints absName cf = unlines $
    ("%start" +++ unwords (map epName eps))
    :
    map typing eps
    where eps = toList $ allEntryPoints cf
          typing :: Cat -> String
          typing c = "%type" +++ "<" ++ qualify (normCat c) ++ ">" +++ epName c
          qualify c = if c `elem` [ TokenCat "Integer", TokenCat "Double", TokenCat "Char",
                                    TokenCat "String", ListCat (TokenCat "Integer"),
                                    ListCat (TokenCat "Double"),
                                    ListCat (TokenCat "Char"),
                                    ListCat (TokenCat "String") ]
                      then fixType c
                      else absName ++ "." ++ fixType c

epName :: Cat -> String
epName c = "p" ++ nonterminal c

entryPointRules :: CF -> String
entryPointRules cf = unlines $ map mkRule $ toList $ allEntryPoints cf
    where
        mkRule :: Cat -> String
        mkRule s = unlines [
            epName s ++ " : " ++ nonterminal s ++ " TOK_EOF { $1 }",
            "  | error { raise (BnfcUtil.ParseError parseState.ResultRange) };"
            ]

rules :: CF -> String
rules cf = unlines [
    entryPointRules cf,
    unlines $ map (prOne . mkOne) (ruleGroups cf),
    specialRules cf
    ]
    where
        mkOne (cat,rules) = (cat, constructRule (terminal cf) rules cat)
        prOne (_,[]) = [] -- nt has only internal use
        prOne (nt, (p,a):ls) =
          unwords [nt', ":" , p, "{", a, "}", "\n" ++ pr ls] ++ ";\n"
         where
           nt' = nonterminal nt
           pr [] = []
           pr ((p,a):ls) =
             unlines [ unwords [ "  |", p, "{", a , "}" ] ] ++ pr ls



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
    f' = case funName f of -- f# cons is somehow not a standard infix oper, right?
           "(:[])" -> "(fun x -> [x])"
           "(:)"   -> "(fun (x,xs) -> x::xs)"
           x       -> sanitizeFSharp x

generatePatterns :: (String -> String) -> Rule -> (Pattern,[MetaVar])
generatePatterns terminal r = case rhsRule r of
  []  -> ("/* empty */",[])
  its -> (unwords (map mkIt its), metas its)
 where
   mkIt i = case i of
     Left c -> nonterminal c
     Right s -> terminal s
   metas its = [ ('$': show i) | (i, Left _c) <- zip [1 ::Int ..] its ]

specialRules :: CF -> String
specialRules cf = unlines $ (`map` literals cf) $ \case
  "Ident"   -> "Ident : TOK_Ident  { Ident $1 };"
  "String"  -> "string : TOK_String { $1 };"
  "Integer" -> "int :  TOK_Integer  { $1 };"
  "Double"  -> "float : TOK_Double  { $1 };"
  "Char"    -> "char : TOK_Char { $1 };"
  own       -> fixType (TokenCat own) ++ " : TOK_" ++ own ++ " { " ++ fixType (TokenCat own) ++ " (" ++  posn ++ "$1)};"
    where -- ignore position categories for now
    posn = "" -- if isPositionCat cf own then "mkPosToken " else ""
