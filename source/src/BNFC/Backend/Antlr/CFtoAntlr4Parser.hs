{-# LANGUAGE LambdaCase #-}

module BNFC.Backend.Antlr.CFtoAntlr4Parser ( cf2AntlrParse ) where

import Data.Foldable ( toList )
import Data.Maybe

import BNFC.CF
import BNFC.Options ( RecordPositions(..) )
import BNFC.Utils   ( (+++), applyWhen )

import BNFC.Backend.Antlr.Utils
import BNFC.Backend.Common.NamedVariables

-- Type declarations

-- | A definition of a non-terminal by all its rhss 
data PDef = PDef
  { _pdNT   :: Maybe String
      -- ^ If given, the name of the lhss.  Usually computed from 'pdCat'.
  , _pdCat  :: Cat
      -- ^ The category to parse.
  , _pdAlts :: [(Pattern, Maybe Fun)]
      -- ^ The possible rhss with actions.  If 'null', skip this 'PDef'.
      --   Where 'Nothing', skip ANTLR rule label.
  }
type Rules       = [PDef]
type Pattern     = String

-- | Creates the ANTLR parser grammar for this CF.
--The environment comes from CFtoAntlr4Lexer
cf2AntlrParse :: String -> CF -> RecordPositions -> KeywordEnv -> String
cf2AntlrParse lang cf _ env = unlines
  [ header
    , tokens
    , ""
    -- Generate start rules
    , prRules $ map entrypoint $ toList $ allEntryPoints cf 
    -- Generate regular rules
    , prRules $ rulesForAntlr4 cf env
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

-- | Generate start rule to help ANTLR.
--
--   @start_X : X EOF
--
entrypoint :: Cat -> PDef
entrypoint cat =
  PDef (Just nt) cat [(pat, fun)]
  where
  nt  = firstLowerCase $ startSymbol $ identCat cat
  pat = catToNT cat +++ "EOF"
  fun = Nothing

--The following functions are a (relatively) straightforward translation
--of the ones in CFtoHappy.hs
rulesForAntlr4 :: CF -> KeywordEnv -> Rules
rulesForAntlr4 cf env = map mkOne getrules
  where
    getrules          = ruleGroups cf
    mkOne (cat,rules) = constructRule cf env rules cat

-- | For every non-terminal, we construct a set of rules. A rule is a sequence of
-- terminals and non-terminals, and an action to be performed.
constructRule :: CF -> KeywordEnv -> [Rule] -> NonTerminal -> PDef
constructRule cf env rules nt =
  PDef Nothing nt $
    [ ( p, Just label )
    | (index, r0) <- zip [1..] rules
    , let b      = isConsFun (funRule r0) && elem (valCat r0) (cfgReversibleCats cf)
    , let r      = applyWhen b revSepListRule r0
    , let p = generatePattern index env r
    , let label  = wpThing (funRule r)
    ]

-- | Generate patterns and a set of metavariables indicating
-- where in the pattern the non-terminal
-- >>> generatePatterns 2 [] $ npRule "myfun" (Cat "A") [] Parsable
-- (" /* empty */ ",[])
-- >>> generatePatterns 3 [("def", "_SYMB_1")] $ npRule "myfun" (Cat "A") [Right "def", Left (Cat "B")] Parsable
-- ("_SYMB_1 p_3_2=b",[("p_3_2",B)])
generatePattern :: Int -> KeywordEnv -> Rule -> Pattern
generatePattern ind env r =
  case rhsRule r of
    []  -> " /* empty */ "
    its ->  unwords $ mapMaybe (uncurry mkIt) nits
      where
      nits   = zip [1 :: Int ..] its
      var i  = "p_" ++ show ind ++"_"++ show i   -- TODO: is ind needed for ANTLR?
      mkIt i = \case
        Left  c -> Just $ var i ++ "=" ++ catToNT c
        Right s -> lookup s env

catToNT :: Cat -> String
catToNT = \case
  TokenCat "Ident"   -> "IDENT"
  TokenCat "Integer" -> "INTEGER"
  TokenCat "Char"    -> "CHAR"
  TokenCat "Double"  -> "DOUBLE"
  TokenCat "String"  -> "STRING"
  c | isTokenCat c   -> identCat c
    | otherwise      -> firstLowerCase $ getRuleName $ identCat c

-- | Puts together the pattern and actions and returns a string containing all
-- the rules.
prRules ::  Rules -> String
prRules = concatMap $ \case

  -- No rules: skip.
  PDef _mlhs _nt []         -> ""

  -- At least one rule: print!
  PDef mlhs nt (rhs : rhss) -> unlines $ concat

    -- The definition header: lhs and type.
    [ [ unwords [fromMaybe nt' mlhs]
      ]
    -- The first rhs.
    , alternative "  :" rhs
    -- The other rhss.
    , concatMap (alternative "  |") rhss
    -- The definition footer.
    , [ "  ;" ]
    ]
    where
    alternative sep (p, label) = unwords [ sep , p ] : [ unwords [ "    #" , antlrRuleLabel l ] | Just l <- [label] ]
      
    catid              = identCat nt
    nt'                = getRuleName $ firstLowerCase catid
    antlrRuleLabel :: Fun -> String
    antlrRuleLabel fnc
      | isNilFun fnc   = catid ++ "_Empty"
      | isOneFun fnc   = catid ++ "_AppendLast"
      | isConsFun fnc  = catid ++ "_PrependFirst"
      | isCoercion fnc = "Coercion_" ++ catid
      | otherwise      = getLabelName fnc
