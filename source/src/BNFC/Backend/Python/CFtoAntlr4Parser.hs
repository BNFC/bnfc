{-
    BNF Converter: Antlr4 Python Generator
    Copyright (C) 2004  Author:  Markus Forsberg, Michael Pellauer,
                                 Bjorn Bringert

    Description   : This module generates the ANTLR .g4 input file for the
                    Python backend. It follows the same basic structure 
                    of CFtoHappy.

    Author        : Gabriele Paganelli (gapag@distruzione.org)
    Created       : 15 Oct, 2015

    Edited for Python 2024 by
                  : Björn Werner
-}

{-# LANGUAGE LambdaCase #-}

module BNFC.Backend.Python.CFtoAntlr4Parser ( cf2AntlrParse ) where

import Data.Foldable ( toList )
import Data.List     ( intercalate )
import Data.Maybe
import BNFC.CF
import BNFC.Utils   ( (+++), (+.+), applyWhen )
import BNFC.Backend.Python.Antlr4Utils
import BNFC.Backend.Common.NamedVariables
import BNFC.Backend.Python.PyHelpers
import Data.Either   (lefts, rights, isLeft)


-- | A definition of a non-terminal by all its rhss,
--   together with parse actions.
data PDef = PDef
  { _pdNT   :: Maybe String
      -- ^ If given, the name of the lhss.  Usually computed from 'pdCat'.
  , _pdCat  :: Cat
      -- ^ The category to parse.
  , _pdAlts :: [(Pattern, Action, Maybe Fun)]
      -- ^ The possible rhss with actions.  If 'null', skip this 'PDef'.
      --   Where 'Nothing', skip ANTLR rule label.
  }
type Rules       = [PDef]
type Pattern     = String
type Action      = String
type MetaVar     = (String, Cat)


-- | Creates the ANTLR parser grammar for this CF.
--The environment comes from CFtoAntlr4Lexer
cf2AntlrParse :: String -> String -> CF -> KeywordEnv -> String
cf2AntlrParse lang packageAbsyn cf env = unlines $ concat
  [ [ header
    , tokens
    , importAbs
    , ""
    -- Generate start rules [#272]
    -- _X returns [ dX result ] : x=X EOF { $result = $x.result; }
    , prRules packageAbsyn $ map entrypoint $ toList $ allEntryPoints cf
    -- Generate regular rules
    , prRules packageAbsyn $ rulesForAntlr4 packageAbsyn cf env
    ]
  ]
  where
    header :: String
    header = unlines
        [ "// Parser definition for use with ANTLRv4"
        , "parser grammar" +++ lang ++ "Parser;"
        ]
    tokens :: String
    tokens = unlines
        [ "options {"
        , "  tokenVocab = " ++ lang ++ "Lexer;"
        , "}"
        ]
    importAbs :: String
    importAbs = unlines
        [ "@parser::header {import " ++ packageAbsyn
        , "}"
        ]


-- | Generate start rule to help ANTLR.
--
--   @start_X returns [ X result ] : x=X EOF { $result = $x.result; } # Start_X@
--
entrypoint :: Cat -> PDef
entrypoint cat =
  PDef (Just nt) cat [(pat, act, fun)]
  where
    nt  = firstLowerCase $ startSymbol $ identCat cat
    pat = "x=" ++ catToNT cat +++ "EOF"
    act = "$result = $x.result;"
    -- No ANTLR Rule label, ("Start_" ++ identCat cat) conflicts with lhs.
    fun = Nothing 


-- | The following functions are a (relatively) straightforward translation
-- of the ones in CFtoHappy.hs
rulesForAntlr4 :: String -> CF -> KeywordEnv -> Rules
rulesForAntlr4 packageAbsyn cf env = map mkOne getrules
  where
    getrules          = ruleGroups cf
    mkOne (cat,rules) = constructRule packageAbsyn cf env rules cat


-- | Aids the pattern constructor for lists
data ListType = Term | Sep | None
  deriving Eq


-- | For every non-terminal, we construct a set of rules. A rule is a 
--   sequence of terminals and non-terminals, and an action to be performed.
--   Complete sets of separator or terminator rules are treated separately, as
--   the default recursive parsing may reach the maximum recursion depth 
--   in Python. Cases of multiple sets of rules are not considered.
constructRule :: String -> CF -> KeywordEnv -> [Rule] -> NonTerminal -> PDef
constructRule packageAbsyn cf env rules nt
  | termOrSep /= None = PDef Nothing nt $ 
    (if oneNilFun then
        [ (" /* empty */ "
          , "$result=[]"
          , Nothing
          )
        ]
      else
        []
    ) ++
    [ ( generateListPatterns packageAbsyn env 
        (rhsRule (head consFuns)) termOrSep oneNilFun
      , "# actions embedded in pattern"
      , Nothing
      )
    ]
  | otherwise = PDef Nothing nt $
    [ ( p
      , generateAction packageAbsyn nt (funRule r) m b
      , Nothing  -- labels not needed for BNFC-generated AST parser
      )
    | (index, r0) <- zip [1..] rules
    , let b = isConsFun (funRule r0) && elem (valCat r0) (cfgReversibleCats cf)
    , let r      = applyWhen b revSepListRule r0
    , let (p,m0) = generatePatterns index env r 
    , let m      = applyWhen b reverse m0
    ]
  where
    -- Figures out if the rules are well formed list rules (using the
    -- separator or terminator pragmas).
    nilFuns = filter isNilFun rules
    oneFuns = filter isOneFun rules
    consFuns = filter isConsFun rules
    
    noNilFuns = length nilFuns == 0
    noOneFuns = length oneFuns == 0

    oneNilFun = length nilFuns == 1
    oneOneFun = length oneFuns == 1
    oneConsFun = length consFuns == 1

    onlyMiddle :: [Either Cat String] -> Bool
    onlyMiddle ecs = all isLeft [head ecs, last ecs]
    
    noStrings :: [Either Cat String] -> Bool
    noStrings ecs = length (rights ecs) == 0
  
    -- Terminator:
    -- []
    -- (:) C ... [C]
    isTerminator = oneNilFun && noOneFuns && oneConsFun && 
      (noStrings . rhsRule . head) nilFuns && 
      (onlyMiddle . rhsRule . head) consFuns

    -- Terminator nonempty:
    -- (:[]) C ...
    -- (:)   C ... [C]
    isTerminatorNonempty = noNilFuns && oneOneFun && oneConsFun && 
      (isLeft . head . rhsRule . head) oneFuns && 
      (onlyMiddle . rhsRule . head) consFuns && 
      (rights . rhsRule . head) oneFuns == ((rights . rhsRule . head) consFuns)
     
    -- Separator:
    -- []
    -- (:[]) C
    -- (:)   C ... [C]
    isSeparator = oneNilFun && oneOneFun && oneConsFun && 
      (noStrings . rhsRule . head) nilFuns && 
      (noStrings . rhsRule . head) oneFuns && 
      (onlyMiddle . rhsRule . head) consFuns

    -- Separator nonempty:
    -- (:[]) C
    -- (:)   C ... [C]
    isSeparatorNonempty = noNilFuns && oneOneFun && oneConsFun && 
      (noStrings . rhsRule . head) oneFuns && 
      (onlyMiddle . rhsRule . head) consFuns

    termOrSep
      | isTerminator || isTerminatorNonempty = Term
      | isSeparator || isSeparatorNonempty = Sep
      | otherwise = None
    

-- | Generates a string containing the semantic action.
generateAction :: IsFun f => String -> NonTerminal -> f -> [MetaVar]
               -> Bool   -- ^ Whether the list should be reversed or not.
                         --   Only used if this is a list rule.
               -> Action
generateAction packageAbsyn _ f ms rev 
  | isNilFun f = "$result =" ++ c ++ ";"
  | isOneFun f = "$result =" ++ c ++ ";; $result.append(" ++ p_1 ++ ")"
  | isConsFun f = "$result =" ++ p_2 ++ ";; $result." ++ add p_1
  | isCoercion f = "$result = " ++  p_1 ++ ";"
  | isDefinedRule f = 
    "$result = " ++ packageAbsyn ++ "." ++ sanitize (funName f)
    ++ "(" ++ intercalate "," (map (resultvalue packageAbsyn) ms) ++ ");"
  | otherwise = "$result = " ++ c
    ++ "(" ++ intercalate "," (map (resultvalue packageAbsyn) ms) ++ ");"
  where
    sanitize          = getRuleName
    c                 = if isNilFun f || isOneFun f || isConsFun f
                         then "[]" 
                         else packageAbsyn ++ "." ++ sanitize (funName f)
    p_1               = resultvalue packageAbsyn $ ms!!0
    p_2               = resultvalue packageAbsyn $ ms!!1
    add p             = (if rev then "append(" else "insert(0, ") ++ p ++ ")"


-- | Gives the abstract syntax constructor for a cat.
resultvalue :: String -> MetaVar -> String
resultvalue packageAbsyn (n,c) = case c of
      TokenCat "Double"  -> concat [ packageAbsyn ++ ".Double(", txt, ")" ]
      TokenCat "Integer" -> concat [ packageAbsyn ++ ".Integer("  , txt, ")" ]
      TokenCat "Char"    -> packageAbsyn ++ ".Char(" ++ txt ++ ")"
      TokenCat "String"  -> packageAbsyn ++ ".String(" ++ txt ++ ")"
      TokenCat "Ident"   -> concat [ packageAbsyn, ".Ident(", txt, ")" ]
      TokenCat s         -> packageAbsyn ++ "." ++ unkw s ++ "(" ++ txt ++ ")"
      _                  -> concat [ "$", n, ".result" ]
      where 
        txt = '$':n +.+ "text"


 -- | Generate patterns and a set of metavariables indicating
-- where in the pattern the non-terminal
-- >>> generatePatterns 2 [] $ npRule "myfun" (Cat "A") [] Parsable
-- (" /* empty */ ",[])
-- >>> generatePatterns 3 [("def", "_SYMB_1")] $ npRule "myfun" (Cat "A") [Right "def", Left (Cat "B")] Parsable
-- ("_SYMB_1 p_3_2=b",[("p_3_2",B)])
generatePatterns :: Int -> KeywordEnv -> Rule -> (Pattern,[MetaVar])
generatePatterns ind env r =
  case rhsRule r of
    []  -> (" /* empty */ ", [])
    its -> ( unwords $ mapMaybe (uncurry mkIt) nits
           , [ (var i, cat) | (i, Left cat) <- nits ]
           )
      where
      nits   = zip [1 :: Int ..] its
      var i  = "p_" ++ show ind ++"_"++ show i
      mkIt i = \case
        Left  c -> Just $ var i ++ "=" ++ catToNT c
        Right s -> lookup s env


-- | Nonempty patterns with embedded semantic actions, to match:
-- Separator: 
--  C (... C)*
-- Terminator:
--  (C ...)+
-- A terminators for example is turned into the pattern:
--  {init list action} ( p_1_2=C {append action} )+
-- Not that for separators with empty, consFun empty is a possible derivation, 
-- meaning a separator can end with delims:
-- C (... C)* (...)?
generateListPatterns :: String -> KeywordEnv -> [Either Cat String] -> 
  ListType -> Bool -> Pattern
generateListPatterns packageAbsyn env ecss termOrSep oneNilFun =
  case termOrSep of
    Sep   -> p1 ++ " {" ++ a1 ++ "} (" ++ delims ++ " " ++ p2 ++ 
      " {" ++ a2 ++ "})*" ++ if oneNilFun then "(" ++ delims ++ ")?" else ""
    Term  -> "{$result=[];} (" ++ p2 ++ " " ++ delims ++ " {" ++  a2 ++ "})+"
    None -> error "Neither Term or Sep"
  where
    c = head (lefts ecss)
    p1 = "p_1_1=" ++ catToNT c
    p2 = "p_1_2=" ++ catToNT c

    a1 = "$result = [" ++ resultvalue packageAbsyn ("p_1_1", c) ++ "]"
    a2 = "$result.append(" ++ resultvalue packageAbsyn ("p_1_2", c) ++ ")"

    delims = unwords (mapMaybe (\s -> lookup s env) (rights ecss)) 


-- | Converts a cat to string, an underscore is appended for keywords words.
catToNT :: Cat -> String
catToNT = \case
  TokenCat "Ident"   -> "IDENT"
  TokenCat "Integer" -> "INTEGER"
  TokenCat "Char"    -> "CHAR"
  TokenCat "Double"  -> "DOUBLE"
  TokenCat "String"  -> "STRING"
  c | isTokenCat c   -> getRuleName $ identCat c
    | otherwise      -> getRuleName $ firstLowerCase $ identCat c


-- | Puts together the pattern and actions and returns a string containing all
-- the rules.
prRules :: String -> Rules -> String
prRules packabs = concatMap $ \case

  -- No rules: skip.
  PDef _mlhs _nt []         -> ""

  -- At least one rule: print!
  PDef mlhs nt (rhs : rhss) -> unlines $ concat

    -- The definition header: lhs and type.
    [ [ unwords [ fromMaybe nt' mlhs
                , "returns" , "[" , packabs+.+normcat , "result" , "]"
                ]
      ]
    -- The first rhs.
    , alternative "  :" rhs
    -- The other rhss.
    , concatMap (alternative "  |") rhss
    -- The definition footer.
    , [ "  ;" ]
    ]
    where
      alternative sep (p, a, label) = concat
        [ [ concat [ sep , p ] ]
        , [ concat [ "    {" , a , "}" ] ]
        , [ concat [ "    #" , antlrRuleLabel l ] | Just l <- [label] ]
        ]
      catid              = identCat nt
      normcat            = identCat (normCat nt)
      nt'                = getRuleName $ firstLowerCase catid
      antlrRuleLabel :: Fun -> String
      antlrRuleLabel fnc
        | isNilFun fnc   = catid ++ "_Empty"
        | isOneFun fnc   = catid ++ "_AppendLast"
        | isConsFun fnc  = catid ++ "_PrependFirst"
        | isCoercion fnc = "Coercion_" ++ catid
        | otherwise      = getLabelName fnc
