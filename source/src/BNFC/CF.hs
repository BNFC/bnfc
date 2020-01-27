{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

{-
    BNF Converter: Abstract syntax
    Copyright (C) 2004  Author:  Markus Forsberg, Michael Pellauer, Aarne Ranta

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1335, USA
-}

module BNFC.CF (
            -- Types.
            CF,
            CFG(..),
            Rule, Rul(..), lookupRule, InternalRule(..),
            Pragma(..),
            Exp(..),
            Base(..), Type(..), Signature,
            Literal,
            Symbol,
            KeyWord,
            Cat(..), strToCat, catToStr,
            TokenCat,
            catString, catInteger, catDouble, catChar, catIdent,
            NonTerminal, SentForm,
            Fun,
            Tree(..),
            prTree,         -- print an abstract syntax tree
            Data,           -- describes the abstract syntax of a grammar
            cf2data,        -- translates a grammar to a Data object.
            cf2dataLists,   -- translates to a Data with List categories included.
            getAbstractSyntax,
            -- Literal categories, constants,
            firstCat,       -- the first value category in the grammar.
            firstEntry,     -- the first entry or the first value category
            baseTokenCatNames,  -- "Char", "Double", "Integer", "String"
            specialCats,    -- ident
            specialCatsP,   -- all literals
            specialData,    -- special data
            isCoercion,     -- wildcards in grammar (avoid syntactic clutter)
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
            sameCat,
            -- Information functions for list functions.
            isNilFun,       -- empty list function? ([])
            isOneFun,       -- one element list function? (:[])
            hasOneFunc,
            getCons,
            getSeparatorByPrecedence,
            isConsFun,      -- constructor function? (:)
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
            precLevels,     -- get all precendence levels in the grammar, sorted in increasing order.
            precRule,       -- get the precendence level of the value category of a rule.
            precCF,         -- Check if the CF consists of precendence levels.
            isUsedCat,
            isPositionCat,
            hasIdent,
            hasLayout,
            layoutPragmas,
            normFun,
            sigLookup,      -- Get the type of a rule label.

            CFP,            -- CF with profiles
            RuleP,
            FunP,
            Prof,
            cf2cfpRule,
            cf2cfp,
            cfp2cf,
            trivialProf,
            funRuleP, ruleGroupsP, allCatsP, allEntryPointsP
           ) where

import Control.Monad (guard)
import Data.Char
import Data.List (nub, intersperse, sort, group, intercalate, find)
import Data.Maybe
import Data.Map  (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import AbsBNF (Reg())
import ParBNF (pCat)
import LexBNF (tokens)
import qualified AbsBNF

import BNFC.Utils (prParenth,(+++))

-- | A context free grammar consists of a set of rules and some extended
-- information (e.g. pragmas, literals, symbols, keywords).

type CF = CFG Fun

-- | A rule consists of a function name, a main category and a sequence of
-- terminals and non-terminals.
-- @
--   function_name . Main_Cat ::= sequence
-- @

type Rule = Rul Fun

-- | Polymorphic rule type for common type signatures for CF and CFP.

data Rul function = Rule
  { funRule :: function
      -- ^ The function (semantic action) of a rule.
      --   In order to be able to generate data types this must be a constructor
      --   (or an identity function).
  , valCat  :: Cat
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

-- | Polymorphic CFG type for common type signatures for CF and CFP.

data CFG function = CFG
    { cfgPragmas        :: [Pragma]
    , cfgLiterals       :: [Literal]  -- ^ @Char, String, Ident, Integer, Double@.
                                      --   @String@s are quoted strings,
                                      --   and @Ident@s are unquoted.
    , cfgSymbols        :: [Symbol]   -- ^ Symbols in the grammar, e.g. “*”, '->'.
    , cfgKeywords       :: [KeyWord]  -- ^ Reserved words, e.g. 'if' 'while'.
    , cfgReversibleCats :: [Cat]      -- ^ Categories that can be made left-recursive.
    , cfgRules          :: [Rul function]
    , cfgSignature      :: Signature  -- ^ Types of rule labels, computed from 'cfgRules'.
    } deriving (Functor)


instance (Show function) => Show (CFG function) where
  show CFG{..} = unlines $ map show cfgRules

-- | Types of the rule labels.
type Signature = Map String Type

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

data Exp
  = App String [Exp]  -- ^ (Possibly defined) label applied to expressions.
  | Var String        -- ^ Function parameter.
  | LitInt Integer
  | LitDouble Double
  | LitChar Char
  | LitString String
  deriving (Eq)

instance Show Exp where
    showsPrec p e =
        case listView e of
            Right es    ->
                showString "["
                . foldr (.) id (intersperse (showString ", ") $ map shows es)
                . showString "]"
            Left (Var x)    -> showString x
            Left (App x []) -> showString x
            Left (App  "(:)" [e1,e2]) ->
                showParen (p>0)
                $ showsPrec 1 e1
                . showString " : "
                . shows e2
            Left (App x es) ->
                showParen (p>1)
                $ foldr (.) id
                $ intersperse (showString " ")
                $ showString x : map (showsPrec 2) es
            Left (LitInt n)     -> shows n
            Left (LitDouble x)  -> shows x
            Left (LitChar c)    -> shows c
            Left (LitString s)  -> shows s
        where
            listView (App "[]" []) = Right []
            listView (App "(:)" [e1,e2])
                | Right es <- listView e2   = Right $ e1:es
            listView x = Left x

-- | Pragmas.

data Pragma = CommentS  String -- ^ for single line comments
            | CommentM (String,String) -- ^  for multiple-line comments.
            | TokenReg String Bool Reg -- ^ for tokens
            | EntryPoints [Cat]
            | Layout [String]
            | LayoutStop [String]
            | LayoutTop
            | FunDef String [String] Exp
            -- ...
              deriving (Show)

-- | User-defined regular expression tokens
tokenPragmas :: CFG f -> [(TokenCat,Reg)]
tokenPragmas cf = [ (name, e) | TokenReg name _ e <- cfgPragmas cf ]

-- | The names of all user-defined tokens.
tokenNames :: CFG f -> [String]
tokenNames cf = map fst (tokenPragmas cf)

layoutPragmas :: CF -> (Bool,[String],[String])
layoutPragmas cf = let ps = cfgPragmas cf in (
  not (null [() | LayoutTop  <- ps]),   -- if there's layout betw top-level
  concat [ss | Layout ss     <- ps],    -- layout-block starting words
  concat [ss | LayoutStop ss <- ps]     -- layout-block ending words
  )

hasLayout :: CF -> Bool
hasLayout cf = case layoutPragmas cf of
  (t,ws,_) -> t || not (null ws)   -- (True,[],_) means: top-level layout only

-- | Literal: builtin-token types Char, String, Ident, Integer, Double.
type Literal = String
type Symbol  = String
type KeyWord = String

------------------------------------------------------------------------------
-- Categories
------------------------------------------------------------------------------

-- | Categories are the non-terminals of the grammar.
data Cat
  = Cat String               -- ^ Ordinary non-terminal.
  | TokenCat TokenCat        -- ^ Token types (like @Ident@, @Integer@, ..., user-defined).
  | ListCat Cat              -- ^ List non-terminals, e.g., @[Ident]@, @[Exp]@, @[Exp1]@.
  | CoercCat String Integer  -- ^ E.g. @Exp1@, @Exp2.
  deriving (Eq, Ord)

type TokenCat = String

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
  where cat2cat (AbsBNF.IdCat (AbsBNF.Identifier i)) =
            case span isDigit (reverse i) of
                ([],c') -> Cat (reverse c')
                (d,c') ->  CoercCat (reverse c') (read (reverse d))
        cat2cat (AbsBNF.ListCat c) = ListCat (cat2cat c)

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

-- | Is this function just a coercion? (I.e. the identity)
isCoercion :: Fun -> Bool
isCoercion = (== "_") -- perhaps this should be changed to "id"?

isDefinedRule :: Fun -> Bool
isDefinedRule (x:_) = isLower x
isDefinedRule [] = error "isDefinedRule: empty function name"

isProperLabel :: Fun -> Bool
isProperLabel f = not (isCoercion f || isDefinedRule f)


-- | FIXME: This is a copy of the old normCat function that some backend use
-- on Fun. Now that the type of Cat has changed, this is no longer possible
-- so this is added for those odd cases. It should be verified if this is
-- really necessary.
normFun :: Fun -> Fun
normFun f = case f of
    '[':cs -> "[" ++ norm (init cs) ++ "]"
    _     -> norm f
  where norm = reverse . dropWhile isDigit . reverse

isNilFun, isOneFun, isConsFun, isNilCons,isConcatFun :: Fun -> Bool
isNilCons f = isNilFun f || isOneFun f || isConsFun f || isConcatFun f
isNilFun    = (== "[]")
isOneFun    = (== "(:[])")
isConsFun   = (== "(:)")
isConcatFun = (== "(++)")

------------------------------------------------------------------------------

-- | Abstract syntax tree.
newtype Tree = Tree (Fun,[Tree])

-- | The abstract syntax of a grammar.
type Data = (Cat, [(Fun,[Cat])])

-- | @firstCat@ returns the first @Cat@egory appearing in the grammar.
firstCat :: CF -> Cat
firstCat = valCat . head . cfgRules

firstEntry :: CF -> Cat
firstEntry cf = case allEntryPoints cf of
                 (x:_) -> x
                 _     -> firstCat cf

-- aggressively ban nonunique names (AR 31/5/2012)

-- | Categories and constructors.
allNames :: CF -> [Fun]
allNames cf = allCatsIdNorm cf ++
  [ f | f <- map funRule $ cfgRules cf
      , not $ isNilCons f
      , not $ isCoercion f
  ]

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
  where unRule (Rule f' c rhs _internal) = (f',(c,rhs))

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
    [r | r <- cfgRules cf, isParsable r, normCat (valCat r) == cat]

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
allCatsIdNorm :: CF -> [String]
allCatsIdNorm = nub . map (identCat . normCat . valCat) . cfgRules

-- | Get all normalized Cat
allCatsNorm :: CF -> [Cat]
allCatsNorm = nub . map (normCat . valCat) . cfgRules

-- | Get all normalized Cat
allParserCatsNorm :: CFG f -> [Cat]
allParserCatsNorm = nub . map normCat . allParserCats

-- | Is the category is used on an rhs?
isUsedCat :: CFG f -> Cat -> Bool
isUsedCat cf cat = cat `elem` [c | r <- cfgRules cf, Left c <- rhsRule r]

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

-- to print parse trees
prTree :: Tree -> String
prTree (Tree (fun,[])) = fun
prTree (Tree (fun,trees)) = fun +++ unwords (map pr2 trees) where
  pr2 t@(Tree (_,ts)) = (if null ts then id else prParenth) (prTree t)




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
        return (f, cs)


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
  mkData (Rule f _ its _) = (f, [normCat c | Left c <- its ])

cf2data :: CF -> [Data]
cf2data = cf2data' $ isDataCat . normCat

cf2dataLists :: CF -> [Data]
cf2dataLists = cf2data' $ isDataOrListCat . normCat

specialData :: CF -> [Data]
specialData cf = [(TokenCat name, [(name, [TokenCat catString])]) | name <- specialCats cf]

-- | Get the type of a rule label.
sigLookup :: Fun -> CF -> Maybe Type
sigLookup f = Map.lookup f . cfgSignature


-- | Checks if the rule is parsable.
isParsable :: Rul f -> Bool
isParsable = (Parsable ==) . internal


-- | Checks if the list has a non-empty rule.
hasOneFunc :: [Rule] -> Bool
hasOneFunc = any (isOneFun . funRule)

-- | Gets the separator for a list.
getCons :: [Rule] -> String
getCons rs = case find (isConsFun . funRule) rs of
    Just (Rule _ _ cats _) -> seper cats
    Nothing              -> error $ "getCons: no construction function found in "
                                  ++ intercalate ", " (map (show . funRule) rs)
  where
    seper [] = []
    seper (Right x:_) = x
    seper (Left _:xs) = seper xs

-- | Helper function that gets the list separator by precedence level
getSeparatorByPrecedence :: [Rule] -> [(Integer,String)]
getSeparatorByPrecedence rules = [ (p, getCons (getRulesFor p)) | p <- precedences ]
  where
    precedences = Set.toDescList . Set.fromList $ map precRule rules
    getRulesFor p = [ r | r <- rules, precRule r == p ]

isEmptyListCat :: CF -> Cat -> Bool
isEmptyListCat cf c = elem "[]" $ map funRule $ rulesForCat' cf c

isNonterm :: Either Cat String -> Bool
isNonterm (Left _) = True
isNonterm (Right _) = False

-- used in Happy to parse lists of form 'C t [C]' in reverse order
-- applies only if the [] rule has no terminals
revSepListRule :: Rul f -> Rul f
revSepListRule (Rule f c ts internal) = Rule f c (xs : x : sep) internal where
  (x,sep,xs) = (head ts, init (tail ts), last ts)
-- invariant: test in findAllReversibleCats have been performed

findAllReversibleCats :: CF -> [Cat]
findAllReversibleCats cf = [c | (c,r) <- ruleGroups cf, isRev c r] where
  isRev c rs = case rs of
     [r1,r2] | isList c -> if isConsFun (funRule r2)
                             then tryRev r2 r1
                           else isConsFun (funRule r1) && tryRev r1 r2
     _ -> False
  tryRev (Rule f _ ts@(x:_:_) _) r = isEmptyNilRule r &&
                                        isConsFun f && isNonterm x && isNonterm (last ts)
  tryRev _ _ = False

isEmptyNilRule :: Rul Fun -> Bool
isEmptyNilRule (Rule f _ ts _) = isNilFun f && null ts

-- | Returns the precedence of a category symbol.
-- E.g.
-- >>> precCat (CoercCat "Abc" 4)
-- 4
precCat :: Cat -> Integer
precCat (CoercCat _ i) = i
precCat (ListCat c) = precCat c
precCat _ = 0

precRule :: Rule -> Integer
precRule = precCat . valCat

precLevels :: CF -> [Integer]
precLevels cf = Set.toAscList $ Set.fromList [ precCat c | c <- reallyAllCats cf]

precCF :: CF -> Bool
precCF cf = length (precLevels cf) > 1


-- | Does the category have a position stored in AST?
isPositionCat :: CFG f -> TokenCat -> Bool
isPositionCat cf cat = or [ b | TokenReg name b _ <- cfgPragmas cf, name == cat]

-- | Grammar with permutation profile à la GF. AR 22/9/2004
type CFP   = CFG FunP
type FunP  = (Fun,Prof)
type RuleP = Rul FunP

-- | Pair of: the original function name, profile
type Prof  = (Fun, [([[Int]],[Int])])

cf2cfp :: CF -> CFP
cf2cfp cfg@CFG{..} = cfg { cfgRules = map cf2cfpRule cfgRules }

cf2cfpRule :: Rule -> RuleP
cf2cfpRule (Rule f c its internal)  = Rule (f, (f, trivialProf its)) c its internal

cfp2cf :: CFP -> CF
cfp2cf = fmap fst

trivialProf :: SentForm -> [([[Int]],[Int])]
trivialProf its = [([],[i]) | (i,_) <- zip [0..] [c | Left c <- its]]

{-# DEPRECATED allCatsP, allEntryPointsP  "Use the version without P postfix instead" #-}

funRuleP :: RuleP -> Fun
funRuleP = fst . funRule

ruleGroupsP :: CFP -> [(Cat,[RuleP])]
ruleGroupsP cf = [(c, rulesForCatP cf c) | c <- allCatsP cf]

rulesForCatP :: CFP -> Cat -> [RuleP]
rulesForCatP cf cat = [r | r <- cfgRules cf, isParsable r, valCat r == cat]

allCatsP :: CFP -> [Cat]
allCatsP = reallyAllCats


-- | Categories that are entry points to the parser
allEntryPoints :: CFG f -> [Cat]
allEntryPoints cf = case concat [cats | EntryPoints cats <- cfgPragmas cf] of
  [] -> allParserCats cf
  cs -> cs

allEntryPointsP :: CFP -> [Cat]
allEntryPointsP = allEntryPoints
