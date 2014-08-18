{-# LANGUAGE PatternGuards, DeriveFunctor, StandaloneDeriving #-}
{-
    BNF Converter: Abstract syntax
    Copyright (C) 2004  Author:  Markus Forberg, Michael Pellauer, Aarne Ranta

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module BNFC.CF (
	    -- Types.
	    CF,
            CFG(..), pragmasOfCF, -- ...
	    Rule, Rul(..), lookupRule,
	    Pragma(..),
	    Exp(..),
	    Literal,
	    Symbol,
	    KeyWord,
            Cat(..), strToCat,
            catString, catInteger, catDouble, catChar, catIdent,
            NonTerminal,
	    Fun,
	    Tree(..),
	    prTree,         -- print an abstract syntax tree
	    Data,           -- describes the abstract syntax of a grammar
	    cf2data,        -- translates a grammar to a Data object.
	    cf2dataLists,   -- translates to a Data with List categories included.
	    -- Literal categories, constants,
	    firstCat,       -- the first value category in the grammar.
	    firstEntry,     -- the first entry or the first value category
	    specialCats,    -- ident
	    specialCatsP,   -- all literals
	    specialData,    -- special data
	    isCoercion,     -- wildcards in grammar (avoid syntactic clutter)
	    isDefinedRule,  -- defined rules (allows syntactic sugar)
	    isProperLabel,  -- not coercion or defined rule
	    allCats,        -- all categories of a grammar
            allCatsNorm,
	    allCatsIdNorm,
	    allEntryPoints,
	    reservedWords,
            cfTokens,
	    symbols,
	    literals,
	    reversibleCats,
	    findAllReversibleCats, -- find all reversible categories
	    identCat,       -- transforms '[C]' to ListC (others, unchanged).
	    isParsable,
	    rulesOfCF,      -- All rules of a grammar.
	    rulesForCat,    -- rules for a given category
	    ruleGroups,     -- Categories are grouped with their rules.
            ruleGroupsInternals, --As above, but includes internal cats.
            notUniqueNames, -- list of not unique names (replaces the following 2)
--	    notUniqueFuns,   -- Returns a list of function labels that are not unique.
--            badInheritence, -- Returns a list of all function labels that can cause problems in languages with inheritence.
	    isList,         -- Checks if a category is a list category.
	    -- Information functions for list functions.
	    isNilFun,       -- empty list function? ([])
	    isOneFun,       -- one element list function? (:[])
            hasOneFunc,
            getCons,
	    isConsFun,      -- constructor function? (:)
	    isNilCons,      -- either three of above?
            isEmptyListCat, -- checks if the list permits []
	    revSepListRule, -- reverse a rule, if it is of form C t [C].
	    normCat,
            isDataCat,
	    normCatOfList,  -- Removes precendence information and enclosed List. C1 => C, C2 => C
	    catOfList,
	    comments,       -- translates the pragmas into two list containing the s./m. comments
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

            CFP,            -- CF with profiles
            RuleP,
	    FunP,
            Prof,
            cf2cfpRule,
            cf2cfp,
            cfp2cf,
            trivialProf,
            rulesOfCFP,
            funRuleP, ruleGroupsP, allCatsP, allEntryPointsP
           ) where

import BNFC.Utils (prParenth,(+++))
import Data.List (nub, intersperse, partition, sort,sort,group,intercalate)
import Data.Char
import AbsBNF (Reg())
import ParBNF (pCat)
import LexBNF (tokens)
import qualified AbsBNF
import ErrM

-- | A context free grammar consists of a set of rules and some extended
-- information (e.g. pragmas, literals, symbols, keywords)
type CF = CFG Fun

-- | A rule consists of a function name, a main category and a sequence of
-- terminals and non-terminals.
-- function_name . Main_Cat ::= sequence
type Rule = Rul Fun

-- | Polymorphic rule type for common type signatures for CF and CFP
data Rul function = Rule { funRule :: function
                           -- ^ The function (semantic action) of a
                           -- rule. In order to be able to generate
                           -- data types this must be a constructor
                           -- (or an identity function).
                         , valCat :: Cat -- ^ The value category
                         , rhsRule :: [Either Cat String]
                           -- ^ The list of Terminals/NonTerminals in
                           -- the right-hand-side of a rule.
                         }
                deriving (Eq,Functor)

instance (Show function) => Show (Rul function) where
  show (Rule f cat rhs) = show f ++ ". " ++ show cat ++ " ::= " ++ intercalate " " (map (either show id) rhs)

-- | Polymorphic CFG type for common type signatures for CF and CFP
newtype CFG function = CFG { unCFG :: (Exts,[Rul function]) }
                deriving (Functor)


instance (Show function) => Show (CFG function) where
  show (CFG (_,rules)) = unlines $ map show rules

type Exts = ([Pragma],Info)

-- | Info is information extracted from the CF, for easy access.
-- Literals - Char, String, Ident, Integer, Double
--            Strings are quoted strings, and Ident are unquoted.
-- Symbols  - symbols in the grammar, e.g. �*�, '->'.
-- KeyWord  - reserved words, e.g. 'if' 'while'
type Info = ([Literal],[Symbol],[KeyWord],[Cat])

-- Expressions for function definitions
data Exp = App String [Exp]
	 | LitInt Integer
	 | LitDouble Double
	 | LitChar Char
	 | LitString String
  deriving (Eq)

instance Show Exp where
    showsPrec p e =
	case listView e of
	    Right es	->
		showString "["
		. foldr (.) id (intersperse (showString ", ") $ map shows es)
		. showString "]"
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
	    Left (LitInt n)	-> shows n
	    Left (LitDouble x)	-> shows x
	    Left (LitChar c)	-> shows c
	    Left (LitString s)	-> shows s
	where
	    listView (App "[]" []) = Right []
	    listView (App "(:)" [e1,e2])
		| Right es <- listView e2   = Right $ e1:es
	    listView e	= Left e

-- | Pragmas
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
tokenPragmas :: CFG f -> [(Cat,Reg)]
tokenPragmas cf = [(Cat name,exp) | TokenReg name _ exp <- pragmasOfCF cf]

-- | The names of all user-defined tokens
tokenNames :: CFG f -> [String]
tokenNames cf = map (show.fst) (tokenPragmas cf)

layoutPragmas :: CF -> (Bool,[String],[String])
layoutPragmas cf = let ps = pragmasOfCF cf in (
  not (null [() | LayoutTop  <- ps]),   -- if there's layout betw top-level
  concat [ss | Layout ss     <- ps],    -- layout-block starting words
  concat [ss | LayoutStop ss <- ps]     -- layout-block ending words
  )

hasLayout :: CF -> Bool
hasLayout cf = case layoutPragmas cf of
  (t,ws,_) -> t || not (null ws)   -- (True,[],_) means: top-level layout only

-- | Literal: Char, String, Ident, Integer, Double
type Literal = Cat
type Symbol  = String
type KeyWord = String

-- | Categories are the Non-terminals of the grammar.
data Cat = InternalCat           -- | Internal category, inserted in 1st
                                      -- position in "internal" rules,
                                      -- essentially ensuring that they are
                                      -- never parsed.
              | Cat String
              | ListCat Cat
              | CoercCat String Integer
  deriving (Eq, Ord)
-- An alias for Cat used in many backends:
type NonTerminal = Cat

-- | Show instance for category symbols
-- >>> show (Cat "Def")
-- "Def"
-- >>> show (ListCat (Cat "Thing"))
-- "[Thing]"
-- >>> show (CoercCat "Expr" 3)
-- "Expr3"
-- >>> show (ListCat (CoercCat "Expr" 2))
-- "[Expr2]"
instance Show Cat where
  show InternalCat = "#"
  show (Cat s) = s
  show (ListCat c) = "[" ++ show c ++ "]"
  show (CoercCat s i) = s ++ show i

-- | Reads a string into a category. This should only needs to handle
-- the case of simple categories (with or without coercion) since list
-- categories are parsed in the grammar already. To be on the safe side here,
-- we still call the parser function that parses categries.
-- >>> strToCat "Abc" == Cat "Abc"
-- True
-- >>> strToCat "Abc123" == CoercCat "Abc" 123
-- True
-- >>> strToCat "[Expr2]" == ListCat (CoercCat "Expr" 2)
-- True
strToCat :: String -> Cat
strToCat "#" = InternalCat
strToCat s =
    case pCat (tokens s) of
        Ok c -> cat2cat c
        Bad e -> Cat s -- error $ "Error parsing cat " ++ s ++ " (" ++ e ++ ")"
                       -- Might be one of the "Internal cat" which are not
                       -- really parsable...
  where cat2cat (AbsBNF.IdCat (AbsBNF.Ident s)) =
            case span isDigit (reverse s) of
                ([],c') -> Cat (reverse c')
	        (d,c') ->  CoercCat (reverse c') (read (reverse d))
        cat2cat (AbsBNF.ListCat c) = ListCat (cat2cat c)

-- Build-in categories contants
catString, catInteger, catDouble, catChar, catIdent :: Cat
catString  = Cat "String"
catInteger = Cat "Integer"
catDouble  = Cat "Double"
catChar    = Cat "Char"
catIdent   = Cat "Ident"

-- | Fun is the function name of a rule.
type Fun     = String
-- | Either Cat or Fun
type Name = String

-- | Abstract syntax tree.
newtype Tree = Tree (Fun,[Tree])

-- | The abstract syntax of a grammar.
type Data = (Cat, [(Fun,[Cat])])

-- | firstCat returns the first Category appearing in the grammar.
firstCat :: CF -> Cat
firstCat = valCat . head . rulesOfCF

firstEntry :: CF -> Cat
firstEntry cf = case allEntryPoints cf of
		 (x:_) -> x
		 _     -> firstCat cf

rulesOfCF   :: CFG f -> [Rul f]
rulesOfCFP  :: CFP -> [RuleP]
infoOfCF    :: CFG f -> Info
pragmasOfCF :: CFG f -> [Pragma]

rulesOfCF   = snd . unCFG
rulesOfCFP  = rulesOfCF
infoOfCF    = snd . fst . unCFG
pragmasOfCF = fst . fst . unCFG

-- aggressively ban nonunique names (AR 31/5/2012)

notUniqueNames :: [Name] -> CF -> [Fun]
notUniqueNames reserved cf = [head xs | xs <- xss, length xs > 1] where
  xss = group (sort names)
  names = reserved ++ allCatsIdNorm cf ++ allFuns cf
  allFuns g = [ f | f <- map funRule (rulesOfCF g), not (isNilCons f || isCoercion f)]

{-# DEPRECATED notUniqueFuns "obsolete" #-}
notUniqueFuns :: CF -> [Fun]
notUniqueFuns cf = let xss = group $ sort [ f | f <- map funRule (rulesOfCF cf),
		                                 not (isNilCons f || isCoercion f)]
		    in [ head xs | xs <- xss, length xs > 1]

badInheritence :: CF -> [Cat]
badInheritence cf = concatMap checkGroup (ruleGroups cf)
 where
  checkGroup (cat, rs) = if (length rs <= 1)
                           then []
                           else case lookupRule (show cat) rs of
                             Nothing -> []
                             Just x -> [cat]

-- extract the comment pragmas.
commentPragmas :: [Pragma] -> [Pragma]
commentPragmas = filter isComment
 where isComment (CommentS _) = True
       isComment (CommentM _) = True
       isComment _            = False

lookupRule :: Eq f => f -> [Rul f] -> Maybe (Cat, [Either Cat String])
lookupRule f = lookup f . map unRule
  where unRule (Rule f c rhs) = (f,(c,rhs))

-- | Returns all normal rules that constructs the given Cat.
rulesForCat :: CF -> Cat -> [Rule]
rulesForCat cf cat = [r | r <- rulesOfCF cf, isParsable r, valCat r == cat]

-- | As rulesForCat, but this version doesn't exclude internal rules.
rulesForCat' :: CF -> Cat -> [Rule]
rulesForCat' cf cat = [r | r <- rulesOfCF cf, valCat r == cat]

-- | Get all categories of a grammar. (No Cat w/o production returned; No duplicates)
allCats :: CFG f -> [Cat]
allCats = nub . map valCat . rulesOfCF

-- | Gets all normalized identified Categories
allCatsIdNorm :: CF -> [String]
allCatsIdNorm = nub . map identCat . map normCat . allCats

-- | Get all normalized Cat
allCatsNorm :: CF -> [Cat]
allCatsNorm = nub . map normCat . allCats

-- | Is the category is used on an rhs?
isUsedCat :: CFG f -> Cat -> Bool
isUsedCat cf cat = elem cat [c | r <- (rulesOfCF cf), Left c <- rhsRule r]

-- | Group all categories with their rules.
ruleGroups :: CF -> [(Cat,[Rule])]
ruleGroups cf = [(c, rulesForCat cf c) | c <- allCats cf]

-- | Group all categories with their rules including internal rules.
ruleGroupsInternals :: CF -> [(Cat,[Rule])]
ruleGroupsInternals cf = [(c, rulesForCat' cf c) | c <- allCats cf]

-- | Get all literals of a grammar. (e.g. String, Double)
literals :: CFG f -> [Cat]
literals cf = lits ++ owns
 where
   (lits,_,_,_) = infoOfCF cf
   owns = map fst (tokenPragmas cf)

{-# DEPRECATED symbols, reservedWords "Almost certainly, you should treat symbols and reserved words uniformly, so use cfTokens instead." #-}

-- | Get all symbols
symbols :: CFG f -> [String]
symbols cf = syms
 where (_,syms,_,_) = infoOfCF cf

-- | Get the keywords of a grammar.
reservedWords :: CFG f -> [String]
reservedWords cf = sort keywords
 where (_,_,keywords,_) = infoOfCF cf

-- | Canonical, numbered list of symbols and reserved words. (These do
-- not end up in the AST.)
cfTokens :: CFG f -> [(String,Int)]
cfTokens cf = zip (sort (symbols cf ++ reservedWords cf)) [1..]
-- NOTE: some backends (incl. Haskell) assume that this list is sorted.

-- | Categories that is left-recursive transformable.
reversibleCats :: CFG f -> [Cat]
reversibleCats cf = cats
  where (_,_,_,cats) = infoOfCF cf

-- | Comments can be defined by the 'comment' pragma
comments :: CF -> ([(String,String)],[String])
comments cf = case commentPragmas (pragmasOfCF cf) of
	       xs -> ([p | CommentM p <- xs],
		      [s | CommentS s <- xs])

-- built-in categories (corresponds to lexer)

-- | Whether the grammar uses the predefined Ident type.
hasIdent :: CFG f -> Bool
hasIdent cf = isUsedCat cf catIdent


-- these need new datatypes

-- | Categories corresponding to tokens. These end up in the
-- AST. (unlike tokens returned by 'cfTokens')
specialCats :: CF -> [Cat]
specialCats cf = (if hasIdent cf then (Cat "Ident":) else id) (map fst (tokenPragmas cf))

-- the parser needs these
specialCatsP :: [Cat]
specialCatsP = map Cat $ words "Ident Integer String Char Double"

-- to print parse trees
prTree :: Tree -> String
prTree (Tree (fun,[])) = fun
prTree (Tree (fun,trees)) = fun +++ unwords (map pr2 trees) where
  pr2 t@(Tree (_,ts)) = (if (null ts) then id else prParenth) (prTree t)

-- abstract syntax trees: data type definitions

cf2data' :: (Cat -> Bool) -> CF -> [Data]
cf2data' predicate cf =
  [(cat, nub (map mkData [r | r <- rulesOfCF cf,
                              let f = funRule r,
			      not (isDefinedRule f),
                              not (isCoercion f), eqCat cat (valCat r)]))
      | cat <- filter predicate (allCats cf)]
 where
  mkData (Rule f _ its) = (f,[normCat c | Left c <- its, c /= InternalCat])

cf2data :: CF -> [Data]
cf2data = cf2data' isDataCat

-- | Does the category correspond to a data type?
isDataCat :: Cat -> Bool
isDataCat c = isDataOrListCat c && not (isList c)

isDataOrListCat :: Cat -> Bool
isDataOrListCat (CoercCat _ _)  = False
isDataOrListCat (Cat ('@':_))   = False
isDataOrListCat _               = True

cf2dataLists :: CF -> [Data]
cf2dataLists = cf2data' isDataOrListCat

specialData :: CF -> [Data]
specialData cf = [(c,[(show c,[Cat "String"])]) | c <- specialCats cf] where


-- to deal with coercions

-- the Haskell convention: the wildcard _ is not a constructor

-- | Is this function just a coercion? (Ie. the identity)
isCoercion :: Fun -> Bool
isCoercion = (== "_") -- perhaps this should be changed to "id"?

isDefinedRule :: Fun -> Bool
isDefinedRule (x:_) = isLower x

isProperLabel :: Fun -> Bool
isProperLabel f = not (isCoercion f || isDefinedRule f)

-- | Categories C1, C2,... (one digit in end) are variants of C. This function
-- returns true if two category are variants of the same abstract category.
-- E.g.
--
-- >>> eqCat (Cat "Abc") (Cat "Abc")
-- True
-- >>> eqCat (CoercCat "Abc" 3) (CoercCat "Abc" 5)
-- True
-- >>> eqCat (CoercCat "Acb" 4) (CoercCat "Abc" 4)
-- False
-- >>> eqCat (Cat "Abc") (CoercCat "Abc" 44)
-- True
eqCat :: Cat -> Cat -> Bool
eqCat (CoercCat c1 _) (CoercCat c2 _) = c1 == c2
eqCat (Cat c1 ) (CoercCat c2 _) = c1 == c2
eqCat (CoercCat c1 _) (Cat c2) = c1 == c2
eqCat c1 c2 = c1 == c2



-- | Removes precendence information. C1 => C, [C2] => [C]
normCat :: Cat -> Cat
normCat (ListCat c) = ListCat (normCat c)
normCat (CoercCat c _) = Cat c
normCat c = c

normCatOfList :: Cat -> Cat
normCatOfList = normCat . catOfList

-- | When given a list Cat, i.e. '[C]', it removes the square
-- brackets, and adds the prefix List, i.e. 'ListC'.  (for Happy and
-- Latex)
-- >>> identCat (ListCat (Cat "C")) -- [C]
-- "ListC"
-- >>> identCat (CoercCat "C" 3)
-- "C3"
identCat :: Cat -> String
identCat (ListCat c) = "List" ++ identCat c
identCat c = show c

-- | FIXME: This is a copy of the old normCat function that some backend use
-- on Fun. Now that the type of Cat has changed, this is no longer possible
-- so this is added for those odd cases. It should be verified if this is
-- really necessary.
normFun :: Fun -> Fun
normFun f = case f of
    '[':cs -> "[" ++ norm (init cs) ++ "]"
    _     -> norm f
  where norm = reverse . dropWhile isDigit . reverse


-- | Checks if the rule is parsable.
isParsable :: Rul f -> Bool
isParsable (Rule _ _ (Left c:_)) = c /= InternalCat
isParsable _ = True

isList :: Cat -> Bool
isList (ListCat _) = True
isList _ = False


-- | Unwraps the list constructor from the category name. Eg. [C1] => C1
-- E.g.
-- >>> catOfList (ListCat (Cat "A"))
-- A
-- >>> catOfList (Cat "B")
-- B
catOfList :: Cat -> Cat
catOfList (ListCat c) = c
catOfList c = c

isNilFun, isOneFun, isConsFun, isNilCons,isConcatFun :: Fun -> Bool
isNilCons f = isNilFun f || isOneFun f || isConsFun f || isConcatFun f
isNilFun f  = f == "[]"
isOneFun f  = f == "(:[])"
isConsFun f = f == "(:)"
isConcatFun f = f == "(++)"

-- | Checks if the list has a non-empty rule.
hasOneFunc :: [Rule] -> Bool
hasOneFunc = any (isOneFun . funRule)

-- | Gets the separator for a list.
getCons :: [Rule] -> String
getCons (Rule f c cats:rs) =
 if isConsFun f
   then seper cats
   else getCons rs
 where
    seper [] = []
    seper ((Right x):xs) = x
    seper ((Left x):xs) = seper xs


isEmptyListCat :: CF -> Cat -> Bool
isEmptyListCat cf c = elem "[]" $ map funRule $ rulesForCat' cf c

isNonterm = either (const True) (const False)

-- used in Happy to parse lists of form 'C t [C]' in reverse order
-- applies only if the [] rule has no terminals
revSepListRule :: Rul f -> Rul f
revSepListRule (Rule f c ts) = Rule f c (xs : x : sep) where
  (x,sep,xs) = (head ts, init (tail ts), last ts)
-- invariant: test in findAllReversibleCats have been performed

findAllReversibleCats :: CF -> [Cat]
findAllReversibleCats cf = [c | (c,r) <- ruleGroups cf, isRev c r] where
  isRev c rs = case rs of
     [r1,r2] | isList c -> if isConsFun (funRule r2)
                             then tryRev r2 r1
                           else if isConsFun (funRule r1)
                             then tryRev r1 r2
                           else False
     _ -> False
  tryRev (Rule f _ ts@(x:_:xs)) r = isEmptyNilRule r &&
                                        isConsFun f && isNonterm x && isNonterm (last ts)
  tryRev _ _ = False

isEmptyNilRule (Rule f _ ts) = isNilFun f && null ts

-- | Returns the precedence of a category symbol.
-- E.g.
--
-- >>> precCat (Cat "Abc")
-- 0
--
-- >>> precCat (CoercCat "Abc" 4)
-- 4
--
-- But!
-- >>> precCat (ListCat (CoercCat "Abc" 2))
-- 0
precCat :: Cat -> Integer
precCat (CoercCat _ i) = i
precCat _ = 0

precRule :: Rule -> Integer
precRule = precCat . valCat

precLevels :: CF -> [Integer]
precLevels cf = sort $ nub $ [ precCat c | c <- allCats cf]

precCF :: CF -> Bool
precCF cf = length (precLevels cf) > 1


-- | Does the category have a position stored in AST?
isPositionCat :: CFG f -> Cat -> Bool
isPositionCat cf cat =  or [b | TokenReg name b _ <- pragmasOfCF cf, Cat name == cat]


-- | Grammar with permutation profile � la GF. AR 22/9/2004
type CFP   = CFG FunP -- (Exts,[RuleP])
type FunP  = (Fun,Prof)
type RuleP = Rul FunP -- (FunP, (Cat, [Either Cat String]))

-- | Pair of: the original function name, profile
type Prof  = (Fun, [([[Int]],[Int])])

cf2cfp :: CF -> CFP
cf2cfp (CFG (es,rs)) = CFG (es, map cf2cfpRule rs)

cf2cfpRule :: Rule -> RuleP
cf2cfpRule (Rule f c its)  = Rule (f, (f, trivialProf its)) c its

cfp2cf :: CFP -> CF
cfp2cf = fmap fst

trivialProf :: [Either Cat String] -> [([[Int]],[Int])]
trivialProf its = [([],[i]) | (i,_) <- zip [0..] [c | Left c <- its]]

{-# DEPRECATED rulesOfCFP, allCatsP, allEntryPointsP  "Use the version without P postfix instead" #-}

funRuleP :: RuleP -> Fun
funRuleP = fst . funRule

ruleGroupsP :: CFP -> [(Cat,[RuleP])]
ruleGroupsP cf = [(c, rulesForCatP cf c) | c <- allCatsP cf]

rulesForCatP :: CFP -> Cat -> [RuleP]
rulesForCatP cf cat = [r | r <- rulesOfCFP cf, isParsable r, valCat r == cat]

allCatsP :: CFP -> [Cat]
allCatsP = allCats


-- | Categories that are entry points to the parser
allEntryPoints :: CFG f -> [Cat]
allEntryPoints cf = case concat [cats | EntryPoints cats <- pragmasOfCF cf] of
  [] -> allCats cf
  cs -> cs

allEntryPointsP :: CFP -> [Cat]
allEntryPointsP = allEntryPoints
