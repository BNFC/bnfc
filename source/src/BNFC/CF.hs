{-# LANGUAGE DeriveTraversable #-}  -- implies DeriveFunctor, DeriveFoldable
{-# LANGUAGE FlexibleInstances #-}  -- implies TypeSynonymInstances
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

{-
    BNF Converter: Abstract syntax
    Copyright (C) 2004  Author:  Markus Forsberg, Michael Pellauer, Aarne Ranta

-}

module BNFC.CF (
            -- Types.
            CF,
            CFG(..),
            Rule, Rul(..), npRule, valCat, lookupRule, InternalRule(..),
            Pragma(..),
            Exp, Exp'(..),
            Base(..), Type(..), Signature,
            Literal,
            Symbol,
            KeyWord,
            LayoutKeyWords, Delimiters(..),
            Position(..), noPosition, prettyPosition, npIdentifier,
            WithPosition(..), blendInPosition,
            RString, RCat,
            Cat(..), strToCat, catToStr,
            BaseCat, TokenCat,
            catString, catInteger, catDouble, catChar, catIdent,
            NonTerminal, SentForm,
            Fun, RFun, IsFun(..),
            Data,           -- describes the abstract syntax of a grammar
            cf2data,        -- translates a grammar to a Data object.
            cf2dataLists,   -- translates to a Data with List categories included.
            getAbstractSyntax,
            -- Literal categories, constants,
            firstEntry,     -- the first entry or the first value category
            baseTokenCatNames,  -- "Char", "Double", "Integer", "String"
            specialCats,    -- ident
            specialCatsP,   -- all literals
            specialData,    -- special data
            isDefinedRule,  -- defined rules (allows syntactic sugar)
            isProperLabel,  -- not coercion or defined rule
            allCats,        -- all categories of a grammar
            allParserCats, allParserCatsNorm,
            reallyAllCats,
            allCatsNorm,
            allCatsIdNorm,
            allEntryPoints,
            reservedWords,
            cfTokens,
            literals,
            findAllReversibleCats, -- find all reversible categories
            identCat,       -- transforms '[C]' to ListC (others, unchanged).
            isParsable,
            rulesForCat,    -- rules for a given category
            rulesForNormalizedCat,    -- rules for a given category
            ruleGroups,     -- Categories are grouped with their rules.
            ruleGroupsInternals, --As above, but includes internal cats.
            allNames,        -- Checking for non-unique names, like @Name. Name ::= Ident;@.
            filterNonUnique,
            isList,         -- Checks if a category is a list category.
            isTokenCat, maybeTokenCat,
            baseCat,
            sameCat,
            -- Information functions for list functions.
            hasNilRule, hasSingletonRule,
            sortRulesByPrecedence,
            isNilCons,      -- either three of above?
            isEmptyListCat, -- checks if the list permits []
            revSepListRule, -- reverse a rule, if it is of form C t [C].
            normCat,
            isDataCat, isDataOrListCat,
            normCatOfList,  -- Removes precendence information and enclosed List. C1 => C, C2 => C
            catOfList,
            comments,       -- translates the pragmas into two list containing the s./m. comments
            numberOfBlockCommentForms,
            tokenPragmas,   -- get the user-defined regular expression tokens
            tokenNames,     -- get the names of all user-defined tokens
            precCat,        -- get the precendence level of a Cat C1 => 1, C => 0
            precRule,       -- get the precendence level of the value category of a rule.
            isUsedCat,
            isPositionCat,
            hasPositionTokens,
            hasIdent, hasIdentLikeTokens,
            hasLayout, hasLayout_,
            layoutPragmas,
            sigLookup      -- Get the type of a rule label.
           ) where

import Control.Arrow ( (&&&) )
import Control.Monad ( guard )
import Data.Char
import Data.Ord      ( Down(..) )
import qualified Data.Either as Either
import Data.Function ( on )
import Data.List     ( nub, intersperse, sort, group, intercalate, find )
import qualified Data.List as List
import Data.List.NonEmpty (pattern (:|), (<|))
import qualified Data.List.NonEmpty as List1
import Data.Maybe
import Data.Map  (Map)
import qualified Data.Map as Map
import Data.Set  (Set)
import qualified Data.Set as Set
import Data.String  (IsString(..))

import BNFC.Abs (Reg())
import BNFC.Par (pCat)
import BNFC.Lex (tokens)
import qualified BNFC.Abs as Abs

import BNFC.PrettyPrint
import BNFC.Utils (headWithDefault, spanEnd)

type List1 = List1.NonEmpty

-- | A context free grammar consists of a set of rules and some extended
-- information (e.g. pragmas, literals, symbols, keywords).

type CF = CFG RFun

-- | A rule consists of a function name, a main category and a sequence of
-- terminals and non-terminals.
--
-- @
--   function_name . Main_Cat ::= sequence
-- @

type Rule = Rul RFun

-- | Polymorphic rule type.

-- N.B.: Was originally made polymorphic for the sake of removed backend --profile.
data Rul function = Rule
  { funRule :: function
      -- ^ The function (semantic action) of a rule.
      --   In order to be able to generate data types this must be a constructor
      --   (or an identity function).
  , valRCat :: RCat
      -- ^ The value category, i.e., the defined non-terminal.
  , rhsRule :: SentForm
      -- ^ The sentential form, i.e.,
      --   the list of (non)terminals in the right-hand-side of a rule.
  , internal :: InternalRule
      -- ^ Is this an "internal" rule only for the AST and printing,
      --   not for parsing?
  } deriving (Eq, Functor)

data InternalRule
  = Internal  -- ^ @internal@ rule (only for AST & printer)
  | Parsable  -- ^ ordinary rule (also for parser)
  deriving (Eq)

instance (Show function) => Show (Rul function) where
  show (Rule f cat rhs internal) = unwords $
    (if internal == Internal then ("internal" :) else id) $
    show f : "." : show cat : "::=" : map (either show id) rhs

-- | A sentential form is a sequence of non-terminals or terminals.
type SentForm = [Either Cat String]

-- | Type of context-free grammars (GFG).

data CFG function = CFG
    { cfgPragmas        :: [Pragma]
    , cfgUsedCats       :: Set Cat    -- ^ Categories used by the parser.
    , cfgLiterals       :: [Literal]  -- ^ @Char, String, Ident, Integer, Double@.
                                      --   @String@s are quoted strings,
                                      --   and @Ident@s are unquoted.
    , cfgSymbols        :: [Symbol]   -- ^ Symbols in the grammar, e.g. “*”, “->”.
    , cfgKeywords       :: [KeyWord]  -- ^ Reserved words, e.g. @if@, @while@.
    , cfgReversibleCats :: [Cat]      -- ^ Categories that can be made left-recursive.
    , cfgRules          :: [Rul function]
    , cfgSignature      :: Signature  -- ^ Types of rule labels, computed from 'cfgRules'.
    } deriving (Functor)


instance (Show function) => Show (CFG function) where
  show CFG{..} = unlines $ map show cfgRules

-- | Types of the rule labels, together with the position of the rule label.
type Signature = Map String (WithPosition Type)

-- | Type of a non-terminal.
data Base = BaseT String
          | ListT Base
    deriving (Eq, Ord)

-- | Type of a rule label.
data Type = FunT [Base] Base
    deriving (Eq, Ord)

instance Show Base where
    show (BaseT x) = x
    show (ListT t) = "[" ++ show t ++ "]"

instance Show Type where
    show (FunT ts t) = unwords $ map show ts ++ ["->", show t]

-- | Expressions for function definitions.

data Exp' f
  = App       f [Exp' f]     -- ^ (Possibly defined) label applied to expressions.
  | Var       String         -- ^ Function parameter.
  | LitInt    Integer
  | LitDouble Double
  | LitChar   Char
  | LitString String
  deriving (Eq)

type Exp = Exp' String

instance (IsFun f, Pretty f) => Pretty (Exp' f) where
  prettyPrec p e =
    case listView e of
      Right es           -> brackets $ hcat $ punctuate ", " $ map (prettyPrec 0) es
      Left (Var x)       -> text x
      Left (App f [])    -> prettyPrec p f
      Left (App f [e1,e2])
        | isConsFun f    -> parensIf (p > 0) $ hsep [ prettyPrec 1 e1, ":", prettyPrec 0 e2 ]
      Left (App f es)    -> parensIf (p > 1) $ hsep $ prettyPrec 1 f : map (prettyPrec 2) es
      Left (LitInt n)    -> (text . show) n
      Left (LitDouble x) -> (text . show) x
      Left (LitChar c)   -> (text . show) c
      Left (LitString s) -> (text . show) s
    where
      listView (App f [])
        | isNilFun f              = Right []
      listView (App f [e1,e2])
        | isConsFun f
        , Right es <- listView e2 = Right $ e1:es
      listView e0                 = Left e0

-- | Pragmas.

data Pragma
  = CommentS  String              -- ^ for single line comments
  | CommentM (String, String)     -- ^  for multiple-line comments.
  | TokenReg RString Bool Reg     -- ^ for tokens
  | EntryPoints [RCat]
  | Layout LayoutKeyWords
  | LayoutStop [KeyWord]
  | LayoutTop Symbol              -- ^ Separator for top-level layout.
  | FunDef RFun [String] Exp

type LayoutKeyWords = [(KeyWord, Delimiters)]

-- | List delimiters.

data Delimiters = Delimiters
  { listSep   :: Symbol           -- ^ List separator.
  , listOpen  :: Symbol           -- ^ List opening delimiter.
  , listClose :: Symbol           -- ^ List closing delimiter.
  } deriving Show

-- | User-defined regular expression tokens
tokenPragmas :: CFG f -> [(TokenCat,Reg)]
tokenPragmas cf = [ (wpThing name, e) | TokenReg name _ e <- cfgPragmas cf ]

-- | The names of all user-defined tokens.
tokenNames :: CFG f -> [String]
tokenNames cf = map fst (tokenPragmas cf)

layoutPragmas :: CF -> (Maybe Symbol, LayoutKeyWords, [KeyWord])
layoutPragmas cf =
  ( listToMaybe [ sep | LayoutTop  sep <- ps ]   -- if there's top-level layout
  , concat      [ kws | Layout     kws <- ps ]   -- layout-block inducing words
  , concat      [ kws | LayoutStop kws <- ps ]   -- layout-block aborting words
  )
  where
  ps = cfgPragmas cf

hasLayout_ :: (Maybe Symbol, LayoutKeyWords, [KeyWord]) -> Bool
hasLayout_ (top, kws, _) = isJust top || not (null kws)   -- (True,[],_) means: top-level layout only

hasLayout :: CF -> Bool
hasLayout = hasLayout_ . layoutPragmas


-- | Literal: builtin-token types Char, String, Ident, Integer, Double.
type Literal = String
type Symbol  = String
type KeyWord = String

------------------------------------------------------------------------------
-- Identifiers with position information
------------------------------------------------------------------------------

-- | Source positions.
data Position
  = NoPosition
  | Position
    { posFile    :: FilePath  -- ^ Name of the grammar file.
    , posLine    :: Int       -- ^ Line in the grammar file.
    , posColumn  :: Int       -- ^ Column in the grammar file.
    } deriving (Show, Eq, Ord)

prettyPosition :: Position -> String
prettyPosition = \case
  NoPosition -> ""
  Position file line col -> List.intercalate ":" [ file, show line, show col, "" ]

data WithPosition a = WithPosition
  { wpPosition :: Position
  , wpThing    :: a
  } deriving (Show, Functor, Foldable, Traversable)

-- | Ignore position in equality and ordering.
instance Eq  a => Eq  (WithPosition a) where (==)    = (==)    `on` wpThing
instance Ord a => Ord (WithPosition a) where compare = compare `on` wpThing

noPosition :: a -> WithPosition a
noPosition = WithPosition NoPosition

-- | A "ranged string" (terminology from Agda code base).
type RString = WithPosition String

-- | Prefix string with pretty-printed position information.
blendInPosition :: RString -> String
blendInPosition (WithPosition pos msg)
  | null p    = msg
  | otherwise = unwords [ p, msg ]
  where
  p = prettyPosition pos

type RCat    = WithPosition Cat

valCat :: Rul fun -> Cat
valCat = wpThing . valRCat

npRule :: Fun -> Cat -> SentForm -> InternalRule -> Rule
npRule f c r internal = Rule (noPosition f) (noPosition c) r internal

npIdentifier :: String -> Abs.Identifier
npIdentifier x = Abs.Identifier ((0, 0), x)

-- identifierName :: Identifier -> String
-- identifierName (Identifier (_, x)) = x

-- identifierPosition :: String -> Identifier -> Position
-- identifierPosition file (Identifier ((line, col), _)) = Position file line col

------------------------------------------------------------------------------
-- Categories
------------------------------------------------------------------------------

-- | Categories are the non-terminals of the grammar.
data Cat
  = Cat String               -- ^ Ordinary non-terminal.
  | TokenCat TokenCat        -- ^ Token types (like @Ident@, @Integer@, ..., user-defined).
  | ListCat Cat              -- ^ List non-terminals, e.g., @[Ident]@, @[Exp]@, @[Exp1]@.
  | CoercCat String Integer  -- ^ E.g. @Exp1@, @Exp2@.
  deriving (Eq, Ord)

type TokenCat = String
type BaseCat  = String

-- An alias for Cat used in many backends:
type NonTerminal = Cat

-- | Render category symbols as strings
catToStr :: Cat -> String
catToStr = \case
  Cat s        -> s
  TokenCat s   -> s
  ListCat c    -> "[" ++ catToStr c ++ "]"
  CoercCat s i -> s ++ show i

instance Show Cat where
  show = catToStr

-- | Reads a string into a category. This should only need to handle
-- the case of simple categories (with or without coercion) since list
-- categories are parsed in the grammar already. To be on the safe side here,
-- we still call the parser function that parses categries.
strToCat :: String -> Cat
strToCat s =
    case pCat (tokens s) of
        Right c -> cat2cat c
        Left _ -> Cat s -- error $ "Error parsing cat " ++ s ++ " (" ++ e ++ ")"
                       -- Might be one of the "Internal cat" which are not
                       -- really parsable...
  where
  cat2cat = \case
    Abs.IdCat (Abs.Identifier (_pos, x))
      | null ds   -> if c `elem` specialCatsP then TokenCat c else Cat c
      | otherwise -> CoercCat c (read ds)
      where (ds, c) = spanEnd isDigit x
    Abs.ListCat c -> ListCat (cat2cat c)

-- Build-in categories contants
catString, catInteger, catDouble, catChar, catIdent :: TokenCat
catString  = "String"
catInteger = "Integer"
catDouble  = "Double"
catChar    = "Char"
catIdent   = "Ident"

-- | Token categories corresponding to base types.
baseTokenCatNames :: [TokenCat]
baseTokenCatNames = [ catChar, catDouble, catInteger, catString ]

-- the parser needs these
specialCatsP :: [TokenCat]
specialCatsP = catIdent : baseTokenCatNames

-- | Does the category correspond to a data type?
isDataCat :: Cat -> Bool
isDataCat c = isDataOrListCat c && not (isList c)

isDataOrListCat :: Cat -> Bool
isDataOrListCat (CoercCat _ _)  = False
isDataOrListCat (Cat ('@':_))   = False
isDataOrListCat (ListCat c)     = isDataOrListCat c
isDataOrListCat _               = True

-- | Categories C1, C2,... (one digit at the end) are variants of C. This function
-- returns true if two category are variants of the same abstract category.
-- E.g.
--
-- >>> sameCat (Cat "Abc") (CoercCat "Abc" 44)
-- True

sameCat :: Cat -> Cat -> Bool
sameCat (CoercCat c1 _) (CoercCat c2 _) = c1 == c2
sameCat (Cat c1)        (CoercCat c2 _) = c1 == c2
sameCat (CoercCat c1 _) (Cat c2)        = c1 == c2
sameCat c1              c2              = c1 == c2

-- | Removes precedence information. C1 => C, [C2] => [C]
normCat :: Cat -> Cat
normCat (ListCat c) = ListCat (normCat c)
normCat (CoercCat c _) = Cat c
normCat c = c

normCatOfList :: Cat -> Cat
normCatOfList = normCat . catOfList

-- | When given a list Cat, i.e. '[C]', it removes the square
-- brackets, and adds the prefix List, i.e. 'ListC'.  (for Happy and
-- Latex)
identCat :: Cat -> String
identCat (ListCat c) = "List" ++ identCat c
identCat c = catToStr c

isList :: Cat -> Bool
isList (ListCat _) = True
isList _           = False

-- | Get the underlying category identifier.
baseCat :: Cat -> Either BaseCat TokenCat
baseCat = \case
  ListCat c    -> baseCat c
  CoercCat x _ -> Left x
  Cat x        -> Left x
  TokenCat x   -> Right x

isTokenCat :: Cat -> Bool
isTokenCat (TokenCat _) = True
isTokenCat _            = False

maybeTokenCat :: Cat -> Maybe TokenCat
maybeTokenCat = \case
  TokenCat c -> Just c
  _          -> Nothing

-- | Unwraps the list constructor from the category name.
--   E.g. @[C1] => C1@.
catOfList :: Cat -> Cat
catOfList (ListCat c) = c
catOfList c = c

------------------------------------------------------------------------------
-- Functions
------------------------------------------------------------------------------

-- | Fun is the function name of a rule.
type Fun     = String
type RFun    = RString

instance IsString RFun where
  fromString = noPosition

class IsFun a where
  funName :: a -> String

  isNilFun    :: a -> Bool   -- ^ Is this the constructor for empty lists?
  isOneFun    :: a -> Bool   -- ^ Is this the constructor for singleton lists?
  isConsFun   :: a -> Bool   -- ^ Is this the list constructor?
  isConcatFun :: a -> Bool   -- ^ Is this list concatenation?

  -- | Is this function just a coercion? (I.e. the identity)
  isCoercion :: a -> Bool

  isNilFun    = funNameSatisfies (== "[]")
  isOneFun    = funNameSatisfies (== "(:[])")
  isConsFun   = funNameSatisfies (== "(:)")
  isConcatFun = funNameSatisfies (== "(++)")
  isCoercion  = funNameSatisfies (== "_")

instance IsFun String where
  funName = id

instance IsFun a => IsFun (WithPosition a) where
  funName = funName . wpThing

instance IsFun a => IsFun (Rul a) where
  funName = funName . funRule

instance IsFun a => IsFun (k, a) where
  funName = funName . snd

funNameSatisfies :: IsFun a => (String -> Bool) -> a -> Bool
funNameSatisfies f = f . funName

isDefinedRule :: IsFun a => a -> Bool
isDefinedRule = funNameSatisfies $ \case
  (x:_) -> isLower x
  []    -> error "isDefinedRule: empty function name"

isProperLabel :: IsFun a => a -> Bool
isProperLabel f = not (isCoercion f || isDefinedRule f)

isNilCons :: IsFun a => a -> Bool
isNilCons f = isNilFun f || isOneFun f || isConsFun f || isConcatFun f

------------------------------------------------------------------------------

-- | The abstract syntax of a grammar.
type Data = (Cat, [(String, [Cat])])

-- | @firstEntry@ returns the first of the @entrypoints@,
--   or (if none), the first parsable @Cat@egory appearing in the grammar.

firstEntry :: CF -> Cat
firstEntry cf = List1.head $ allEntryPoints cf

-- aggressively ban nonunique names (AR 31/5/2012)

-- | Constructors and categories.
allNames :: CF -> [RString]
allNames cf =
  [ f | f <- map funRule $ cfgRules cf
      , not $ isNilCons f
      , not $ isCoercion f
  ] ++
  allCatsIdNorm cf
    -- Put the categories after the labels so that the error location
    -- for a non-unique name is at the label rather than the category.

-- | Get all elements with more than one occurrence.
filterNonUnique :: (Ord a) => [a] -> [a]
filterNonUnique xs = [ x | (x:_:_) <- group $ sort xs ]


-- | Extract the comment pragmas.
commentPragmas :: [Pragma] -> [Pragma]
commentPragmas = filter isComment
 where isComment (CommentS _) = True
       isComment (CommentM _) = True
       isComment _            = False

lookupRule :: Eq f => f -> [Rul f] -> Maybe (Cat, SentForm)
lookupRule f = lookup f . map unRule
  where unRule (Rule f' c rhs _internal) = (f', (wpThing c, rhs))

-- | Returns all parseable rules that construct the given Cat.
--   Whitespace separators have been removed.
rulesForCat :: CF -> Cat -> [Rule]
rulesForCat cf cat =
  [ removeWhiteSpaceSeparators r | r <- cfgRules cf, isParsable r, valCat r == cat]

removeWhiteSpaceSeparators :: Rul f -> Rul f
removeWhiteSpaceSeparators = mapRhs $ mapMaybe $ either (Just . Left) $ \ sep ->
  if all isSpace sep then Nothing else Just (Right sep)

-- | Modify the 'rhsRule' part of a 'Rule'.
mapRhs :: (SentForm -> SentForm) -> Rul f -> Rul f
mapRhs f r = r { rhsRule = f $ rhsRule r }

-- | Like rulesForCat but for normalized value categories.
-- I.e., `rulesForCat (Cat "Exp")` will return rules for category Exp but also
-- Exp1, Exp2... in case of coercion
rulesForNormalizedCat :: CF -> Cat -> [Rule]
rulesForNormalizedCat cf cat =
    [r | r <- cfgRules cf, normCat (valCat r) == cat]

-- | As rulesForCat, but this version doesn't exclude internal rules.
rulesForCat' :: CF -> Cat -> [Rule]
rulesForCat' cf cat = [r | r <- cfgRules cf, valCat r == cat]

-- | Get all categories of a grammar matching the filter.
--   (No Cat w/o production returned; no duplicates.)
allCats :: (InternalRule -> Bool) -> CFG f -> [Cat]
allCats pred = nub . map valCat . filter (pred . internal) . cfgRules

-- | Get all categories of a grammar.
--   (No Cat w/o production returned; no duplicates.)
reallyAllCats :: CFG f -> [Cat]
reallyAllCats = allCats $ const True

allParserCats :: CFG f -> [Cat]
allParserCats = allCats (== Parsable)

-- | Gets all normalized identified Categories
allCatsIdNorm :: CF -> [RString]
allCatsIdNorm = nub . map (fmap (identCat . normCat) . valRCat) . cfgRules

-- | Get all normalized Cat
allCatsNorm :: CF -> [Cat]
allCatsNorm = nub . map (normCat . valCat) . cfgRules

-- | Get all normalized Cat
allParserCatsNorm :: CFG f -> [Cat]
allParserCatsNorm = nub . map normCat . allParserCats

-- | Is the category is used on an rhs?
--   Includes internal rules.
isUsedCat :: CFG f -> Cat -> Bool
isUsedCat cf = (`Set.member` cfgUsedCats cf)

-- | Group all parsable categories with their rules.
--   Deletes whitespace separators, as they will not become part of the parsing rules.
ruleGroups :: CF -> [(Cat,[Rule])]
ruleGroups cf = [(c, rulesForCat cf c) | c <- allParserCats cf]

-- | Group all categories with their rules including internal rules.
ruleGroupsInternals :: CF -> [(Cat,[Rule])]
ruleGroupsInternals cf = [(c, rulesForCat' cf c) | c <- reallyAllCats cf]

-- | Get all literals of a grammar. (e.g. String, Double)
literals :: CFG f -> [TokenCat]
literals cf = cfgLiterals cf ++ map fst (tokenPragmas cf)

-- | Get the keywords of a grammar.
reservedWords :: CFG f -> [String]
reservedWords = sort . cfgKeywords

-- | Canonical, numbered list of symbols and reserved words. (These do
-- not end up in the AST.)
cfTokens :: CFG f -> [(String,Int)]
cfTokens cf = zip (sort (cfgSymbols cf ++ reservedWords cf)) [1..]
-- NOTE: some backends (incl. Haskell) assume that this list is sorted.

-- | Comments can be defined by the 'comment' pragma
comments :: CF -> ([(String,String)],[String])
comments cf = ([p | CommentM p <- xs], [s | CommentS s <- xs])
  where
  xs = commentPragmas (cfgPragmas cf)

-- | Number of block comment forms defined in the grammar file.
numberOfBlockCommentForms :: CF -> Int
numberOfBlockCommentForms = length . fst . comments


-- built-in categories (corresponds to lexer)

-- | Whether the grammar uses the predefined Ident type.
hasIdent :: CFG f -> Bool
hasIdent cf = isUsedCat cf $ TokenCat catIdent


-- these need new datatypes

-- | Categories corresponding to tokens. These end up in the
-- AST. (unlike tokens returned by 'cfTokens')
specialCats :: CF -> [TokenCat]
specialCats cf = (if hasIdent cf then (catIdent:) else id) (map fst (tokenPragmas cf))


-- * abstract syntax trees: data type definitions
--
-- The abstract syntax, instantiated by the Data type, is the type signatures
-- of all the constructors.

-- | Return the abstract syntax of the grammar.
-- All categories are normalized, so a rule like:
--     EAdd . Exp2 ::= Exp2 "+" Exp3 ;
-- Will give the following signature: EAdd : Exp -> Exp -> Exp
getAbstractSyntax :: CF -> [Data]
getAbstractSyntax cf = [ ( c, nub (constructors c) ) | c <- allCatsNorm cf ]
  where
    constructors cat = do
        rule <- cfgRules cf
        let f = funRule rule
        guard $ not (isDefinedRule f)
        guard $ not (isCoercion f)
        guard $ normCat (valCat rule) == cat
        let cs = [normCat c | Left c <- rhsRule rule ]
        return (wpThing f, cs)


-- | All the functions below implement the idea of getting the
-- abstract syntax of the grammar with some variation but they seem to do a
-- poor job at handling corner cases involving coercions.
-- Use 'getAbstractSyntax' instead if possible.

cf2data' :: (Cat -> Bool) -> CF -> [Data]
cf2data' predicate cf =
  [(cat, nub (map mkData [r | r <- cfgRules cf,
                              let f = funRule r,
                              not (isDefinedRule f),
                              not (isCoercion f), sameCat cat (valCat r)]))
      | cat <- nub $ map normCat $ filter predicate $ reallyAllCats cf ]
 where
  mkData (Rule f _ its _) = (wpThing f, [normCat c | Left c <- its ])

cf2data :: CF -> [Data]
cf2data = cf2data' $ isDataCat . normCat

cf2dataLists :: CF -> [Data]
cf2dataLists = cf2data' $ isDataOrListCat . normCat

specialData :: CF -> [Data]
specialData cf = [(TokenCat name, [(name, [TokenCat catString])]) | name <- specialCats cf]

-- | Get the type of a rule label.
sigLookup :: IsFun a => a -> CF -> Maybe (WithPosition Type)
sigLookup f = Map.lookup (funName f) . cfgSignature


-- | Checks if the rule is parsable.
isParsable :: Rul f -> Bool
isParsable = (Parsable ==) . internal

hasNilRule :: [Rule] -> Maybe Rule
hasNilRule = List.find isNilFun

-- | Gets the singleton rule out of the rules for a list.
hasSingletonRule :: [Rule] -> Maybe Rule
hasSingletonRule = List.find isOneFun

-- | Sort rules by descending precedence.

sortRulesByPrecedence :: [Rule] -> [(Integer,Rule)]
sortRulesByPrecedence = List.sortOn (Down . fst) . map (precRule &&& id)

-- | Is the given category a list category parsing also empty lists?
isEmptyListCat :: CF -> Cat -> Bool
isEmptyListCat cf = any isNilFun . rulesForCat' cf

isNonterm :: Either Cat String -> Bool
isNonterm = Either.isLeft

-- used in Happy to parse lists of form 'C t [C]' in reverse order
-- applies only if the [] rule has no terminals
revSepListRule :: Rul f -> Rul f
revSepListRule (Rule f c ts internal) = Rule f c (xs : x : sep) internal where
  (x,sep,xs) = (head ts, init (tail ts), last ts)
-- invariant: test in findAllReversibleCats have been performed

findAllReversibleCats :: CF -> [Cat]
findAllReversibleCats cf = [c | (c,r) <- ruleGroups cf, isRev c r]
  where
  isRev c = \case
     [r1,r2] | isList c -> if isConsFun (funRule r2) then tryRev r2 r1
                           else isConsFun (funRule r1) && tryRev r1 r2
     _ -> False
  tryRev (Rule f _ ts@(x:_:_) _) r = isEmptyNilRule r &&
                                        isConsFun f && isNonterm x && isNonterm (last ts)
  tryRev _ _ = False

isEmptyNilRule :: IsFun a => Rul a -> Bool
isEmptyNilRule (Rule f _ ts _) = isNilFun f && null ts

-- | Returns the precedence of a category symbol.
-- E.g.
-- >>> precCat (CoercCat "Abc" 4)
-- 4
precCat :: Cat -> Integer
precCat (CoercCat _ i) = i
precCat (ListCat c) = precCat c
precCat _ = 0

precRule :: Rul f -> Integer
precRule = precCat . valCat

-- | Defines or uses the grammar token types like @Ident@?
--   Excludes position tokens.
hasIdentLikeTokens :: CFG g -> Bool
hasIdentLikeTokens cf = hasIdent cf || or [ not b | TokenReg _ b _ <- cfgPragmas cf ]

-- | Is there a @position token@ declaration in the grammar?
hasPositionTokens :: CFG g -> Bool
hasPositionTokens cf = or [ b | TokenReg _ b _ <- cfgPragmas cf ]

-- | Does the category have a position stored in AST?
isPositionCat :: CFG f -> TokenCat -> Bool
isPositionCat cf cat = or [ b | TokenReg name b _ <- cfgPragmas cf, wpThing name == cat]


-- | Categories that are entry points to the parser.
--
--   These are either the declared @entrypoints@ (in the original order),
--   or, if no @entrypoints@ were declared explicitly,
--   all parsable categories (in the order of declaration in the grammar file).
allEntryPoints :: CFG f -> List1 Cat
allEntryPoints cf =
  case concat [ cats | EntryPoints cats <- cfgPragmas cf ] of
    []   -> List1.fromList $ allParserCats cf  -- assumed to be non-empty
    c:cs -> fmap wpThing (c :| cs)
