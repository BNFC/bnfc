{-# LANGUAGE LambdaCase #-}

module BNFC.Backend.Antlr.CFtoAntlr4Parser ( cf2AntlrParse, antlrRuleLabel, makeLeftRecRule ) where

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
    , let r = makeLeftRecRule cf r0
    , let p = generatePattern index env r
    , let label  = wpThing (funRule r)
    ]

makeLeftRecRule :: CF -> Rule -> Rule
makeLeftRecRule cf rule = applyWhen canBeLeftRecursive revSepListRule rule
  where
    canBeLeftRecursive = isConsFun (funRule rule) && elem (valCat rule) (cfgReversibleCats cf)

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
  PDef mlhs nt rhss -> unlines $ concat

    -- The definition header: lhs and type.
    [ [ unwords [fromMaybe nt' mlhs]
      ]
    -- The first rhs.
    , alternative "  :" $ head indexedRhss
    -- The other rhss.
    , concatMap (alternative "  |") $ tail indexedRhss
    -- The definition footer.
    , [ "  ;" ]
    ]
    where
    alternative sep ((p, label), idx) = unwords [ sep , p ] : [ unwords [ "    #" , antlrRuleLabel nt l idx] | Just l <- [label] ]
    indexedRhss = zipWith (\rule idx -> if (maybe False isCoercion (snd rule)) then (rule, Just idx) else (rule, Nothing)) rhss [1..]
      
    catid              = identCat nt
    nt'                = getRuleName $ firstLowerCase catid

-- we use rule's index as prefix for ANTLR label
-- in order to avoid name collisions for coercion types
antlrRuleLabel :: Cat -> Fun -> Maybe Integer -> String
antlrRuleLabel cat fnc int
  | isNilFun fnc   = catid ++ "_Empty"
  | isOneFun fnc   = catid ++ "_AppendLast"
  | isConsFun fnc  = catid ++ "_PrependFirst"
  | isCoercion fnc = "Coercion_" ++ catid ++ maybe "" (("_" ++) . show) int
  | otherwise      = getLabelName fnc
  where
    catid = identCat cat
