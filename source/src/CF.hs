{-# OPTIONS -fglasgow-exts #-}
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

module CF (
	    -- Types.
	    CF,
	    Rule,
	    Pragma(..),
	    Exp(..),
	    Literal,
	    Symbol,
	    KeyWord,
	    Cat,
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
	    allCatsIdNorm,
	    allEntryPoints, -- those categories that are entry points to the parser
	    reservedWords,  -- get the keywords of a grammar.
	    symbols,        -- get all symbols
	    literals,       -- get all literals of a grammar. (e.g. String, Double)
	    reversibleCats, -- categories that is left-recursive transformable.
	    findAllReversibleCats, -- find all reversible categories
	    identCat,       -- transforms '[C]' to ListC (others, unchanged).
	    valCat,         -- The value category of a rule.
	    isParsable,     -- Checks if the rule is parsable.
	    rulesOfCF,      -- All rules of a grammar.
	    rulesForCat,    -- rules for a given category
	    ruleGroups,     -- Categories are grouped with their rules.
            ruleGroupsInternals, --As above, but includes internal cats.
	    funRule,        -- The function name of a rule.
	    notUniqueFuns,   -- Returns a list of function labels that are not unique.
            badInheritence, -- Returns a list of all function labels that can cause problems in languages with inheritence.
	    isList,         -- Checks if a category is a list category.
	    -- Information functions for list functions.
	    isNilFun,       -- empty list function? ([])
	    isOneFun,       -- one element list function? (:[])
	    isConsFun,      -- constructor function? (:)
	    isNilCons,      -- either three of above?
            isEmptyListCat, -- checks if the list permits []
	    revSepListRule, -- reverse a rule, if it is of form C t [C].
	    rhsRule,        -- The list of Terminals/NonTerminals of a rule.
	    normCat,        -- Removes precendence information. C1 => C, [C2] => [C]
	    normCatOfList,  --   Removes precendence information and enclosed List. C1 => C, C2 => C
	    catOfList,	    -- Removes enclosed list: [C1] => C1
	    comments,       -- translates the pragmas into two list containing the s./m. comments
            tokenPragmas,   -- user-defined regular expression tokens
            tokenNames,     -- The names of all user-defined tokens
	    precCat,        -- get the precendence level of a Cat C1 => 1, C => 0
	    precLevels,     -- get all precendence levels in the grammar, sorted in increasing order.
	    precRule,       -- get the precendence level of the value category of a rule.
	    precCF,         -- Check if the CF consists of precendence levels.
            isUsedCat,
	    internalCat,    -- the symbol #
            isPositionCat,  -- category that has a position in AST
            hasIdent,
            hasLayout,
            layoutPragmas,
            checkRule,

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

import Utils (prParenth,(+++))
import Data.List (nub, intersperse, partition, sort,sort,group)
import Data.Char
import AbsBNF (Reg())

-- A context free grammar consists of a set of rules and some extended
-- information (e.g. pragmas, literals, symbols, keywords)
type CF = (Exts,[Rule])

-- A rule consists of a function name, a main category and a sequence of
-- terminals and non-terminals.
-- function_name . Main_Cat ::= sequence
type Rule = (Fun, (Cat, [Either Cat String]))

-- polymorphic types for common type signatures for CF and CFP
type Rul f = (f, (Cat, [Either Cat String]))
type CFG f = (Exts,[Rul f])

type Exts = ([Pragma],Info)
-- Info is information extracted from the CF, for easy access.
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

instance Show Exp where
    showsPrec p e =
	case listView e of
	    Right es	->
		showString "["
		. foldr (.) id (intersperse (showString ", ") $ map shows es)
		. showString "]"
	    Left (App x []) -> showString x
	    Left (App "(:)" [e1,e2]) ->
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

-- pragmas for single line comments and for multiple-line comments.
data Pragma = CommentS  String
            | CommentM (String,String)
            | TokenReg String Bool Reg
            | EntryPoints [Cat]
            | Layout [String]
            | LayoutStop [String]
            | LayoutTop
	    | FunDef String [String] Exp
            -- ...
	      deriving (Show)

tokenPragmas :: CFG f -> [(String,Reg)]
tokenPragmas cf = [(name,exp) | TokenReg name _ exp <- pragmasOfCF cf]

tokenNames :: CF -> [String]
tokenNames cf = fst (unzip (tokenPragmas cf))

layoutPragmas :: CF -> (Bool,[String],[String])
layoutPragmas cf = let ps = pragmasOfCF cf in (
  not (null [() | LayoutTop  <- ps]),   -- if there's layout betw top-level
  concat [ss | Layout ss     <- ps],    -- layout-block starting words
  concat [ss | LayoutStop ss <- ps]     -- layout-block ending words
  )

hasLayout :: CF -> Bool
hasLayout cf = case layoutPragmas cf of
  (t,ws,_) -> t || not (null ws)   -- (True,[],_) means: top-level layout only

-- Literal: Char, String, Ident, Integer, Double
type Literal = Cat
type Symbol  = String
type KeyWord = String

-- Cat is the Non-terminals of the grammar.
type Cat     = String
-- Fun is the function name of a rule.
type Fun     = String

internalCat :: Cat
internalCat = "#"

-- Abstract syntax tree.
newtype Tree = Tree (Fun,[Tree])

-- The abstract syntax of a grammar.
type Data = (Cat, [(Fun,[Cat])])

-- firstCat returns the first Category appearing in the grammar.
firstCat :: CF -> Cat
firstCat = valCat . head . rulesOfCF

firstEntry :: CF -> Cat
firstEntry cf = case allEntryPoints cf of
		 (x:_) -> x
		 _     -> firstCat cf

rulesOfCF :: CF -> [Rule]
rulesOfCF = snd

notUniqueFuns :: CF -> [Fun]
notUniqueFuns cf = let xss = group $ sort [ f | (f,_) <- rulesOfCF cf,
		                                 not (isNilCons f || isCoercion f)]
		    in [ head xs | xs <- xss, length xs > 1]

badInheritence :: CF -> [Cat]
badInheritence cf = concatMap checkGroup (ruleGroups cf)
 where
  checkGroup (cat, rs) = if (length rs <= 1)
                           then []
                           else case lookup cat rs of
                             Nothing -> []
                             Just x -> [cat]

infoOfCF :: CFG f -> Info
infoOfCF = snd . fst

pragmasOfCF :: CFG f -> [Pragma]
pragmasOfCF = fst . fst

-- extract the comment pragmas.
commentPragmas :: [Pragma] -> [Pragma]
commentPragmas = filter isComment
 where isComment (CommentS _) = True
       isComment (CommentM _) = True
       isComment _            = False

-- returns all normal rules that constructs the given Cat.
rulesForCat :: CF -> Cat -> [Rule]
rulesForCat cf cat = [normRuleFun r | r <- rulesOfCF cf, isParsable r, valCat r == cat]

--This version doesn't exclude internal rules.
rulesForCat' :: CF -> Cat -> [Rule]
rulesForCat' cf cat = [normRuleFun r | r <- rulesOfCF cf, valCat r == cat]

valCat :: Rul f -> Cat
valCat = fst . snd

-- Get all categories of a grammar.
allCats :: CF -> [Cat]
allCats = nub . map valCat . rulesOfCF -- no cats w/o production

-- Gets all normalized identified Categories
allCatsIdNorm :: CF -> [Cat]
allCatsIdNorm = nub . map identCat . map normCat . allCats

-- category is used on an rhs
isUsedCat :: CF -> Cat -> Bool
isUsedCat cf cat = elem cat [c | r <- (rulesOfCF cf), Left c <- rhsRule r]

-- entry points to parser ----
allEntryPoints :: CF -> [Cat]
allEntryPoints cf = case concat [cats | EntryPoints cats <- pragmasOfCF cf] of
  [] -> allCats cf
  cs -> cs

-- group all categories with their rules.
ruleGroups :: CF -> [(Cat,[Rule])]
ruleGroups cf = [(c, rulesForCat cf c) | c <- allCats cf]

-- group all categories with their rules including internal rules.
ruleGroupsInternals :: CF -> [(Cat,[Rule])]
ruleGroupsInternals cf = [(c, rulesForCat' cf c) | c <- allCats cf]

literals :: CFG f -> [Cat]
literals cf = lits ++ owns
 where
   (lits,_,_,_) = infoOfCF cf
   owns = map fst $ tokenPragmas cf

symbols :: CFG f -> [String]
symbols cf = syms
 where (_,syms,_,_) = infoOfCF cf

reservedWords :: CFG f -> [String]
reservedWords cf = sort keywords
 where (_,_,keywords,_) = infoOfCF cf

reversibleCats :: CFG f -> [Cat]
reversibleCats cf = cats
  where (_,_,_,cats) = infoOfCF cf

-- Comments can be defined by the 'comment' pragma
comments :: CF -> ([(String,String)],[String])
comments cf = case commentPragmas (pragmasOfCF cf) of
	       xs -> ([p | CommentM p <- xs],
		      [s | CommentS s <- xs])

funRule :: Rule -> Fun
funRule = fst

rhsRule :: Rul f -> [Either Cat String]
rhsRule = snd . snd

-- built-in categories (corresponds to lexer)

-- if the gramamr uses the predefined Ident type
hasIdent :: CF -> Bool
hasIdent cf = isUsedCat cf "Ident"

-- these need new datatypes
specialCats :: CF -> [Cat]
specialCats cf = (if hasIdent cf then ("Ident":) else id) (map fst (tokenPragmas cf))

-- the parser needs these
specialCatsP :: [Cat]
specialCatsP = words "Ident Integer String Char Double"

-- to print parse trees
prTree :: Tree -> String
prTree (Tree (fun,[])) = fun
prTree (Tree (fun,trees)) = fun +++ unwords (map pr2 trees) where
  pr2 t@(Tree (_,ts)) = (if (null ts) then id else prParenth) (prTree t)

-- abstract syntax trees: data type definitions

cf2data :: CF -> [Data]
cf2data cf =
  [(cat, nub (map mkData [r | r@(f,_) <- rulesOfCF cf,
			      not (isDefinedRule f),
                              not (isCoercion f), eqCat cat (valCat r)]))
      | cat <- allNormalCats cf]
 where
  mkData (f,(_,its)) = (normFun f,[normCat c | Left c <- its, c /= internalCat])

--This version includes lists in the returned data.
--Michael 4/03
cf2dataLists :: CF -> [Data]
cf2dataLists cf =
  [(cat, nub (map mkData [r | r@(f,_) <- rulesOfCF cf,
			      not (isDefinedRule f),
                              not (isCoercion f), eqCat cat (valCat r)]))
      | cat <- (filter (\x -> not $ isDigit $ last x) (allCats cf))]
 where
  mkData (f,(_,its)) = (normFun f,[normCat c | Left c <- its, c /= internalCat])

specialData :: CF -> [Data]
specialData cf = [(c,[(c,[arg c])]) | c <- specialCats cf] where
  arg c = case c of
    _ -> "String"

allNormalCats :: CF -> [Cat]
allNormalCats = filter isNormal . allCats

-- to deal with coercions

-- the Haskell convention: the wildcard _ is not a constructor

isCoercion :: Fun -> Bool
isCoercion = (== "_")

isDefinedRule :: Fun -> Bool
isDefinedRule (x:_) = isLower x

isProperLabel :: Fun -> Bool
isProperLabel f = not (isCoercion f || isDefinedRule f)

-- categories C1, C2,... (one digit in end) are variants of C

eqCat :: Cat -> Cat -> Bool
eqCat c c1 = catCat c == catCat c1

normCat :: Cat -> Cat
normCat c = case c of
  '[':cs -> "[" ++ norm (init cs) ++ "]"
  _     -> unList $ norm c -- to be deprecated
 where
   norm = reverse . dropWhile isDigit . reverse

normCatOfList :: Cat -> Cat
normCatOfList = normCat . catOfList

-- for Happy and Latex
-- When given a list Cat, i.e. '[C]', it removes the square brackets,
-- and adds the prefix List, i.e. 'ListC'.
identCat :: Cat -> Cat
identCat c = case c of
  '[':cs -> "List" ++ identCat (init cs)
  _ -> c

normFun :: Fun -> Fun
normFun = id -- takeWhile (not . isDigit)

normRuleFun :: Rule -> Rule
normRuleFun (f,p) = (normFun f, p)

isNormal :: Cat -> Bool
isNormal c = not (isList c || isDigit (last c))

isParsable :: Rul f -> Bool
isParsable (_,(_, Left "#":_)) = False
isParsable _ = True

isList :: Cat -> Bool
isList c = head c == '['

unList :: Cat -> Cat
unList c = c

catOfList :: Cat -> Cat
catOfList c = case c of
  '[':_:_ -> init (tail c)
  _ -> c

isNilFun, isOneFun, isConsFun, isNilCons :: Fun -> Bool
isNilCons f = isNilFun f || isOneFun f || isConsFun f
isNilFun f  = f == "[]"
isOneFun f  = f == "(:[])"
isConsFun f = f == "(:)"

isEmptyListCat :: CF -> Cat -> Bool
isEmptyListCat cf c = elem "[]" $ map fst $ rulesForCat' cf c

isNonterm = either (const True) (const False)

-- used in Happy to parse lists of form 'C t [C]' in reverse order
-- applies only if the [] rule has no terminals
revSepListRule :: Rul f -> Rul f
revSepListRule r@(f,(c, ts)) = (f, (c, xs : x : sep)) where
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
  tryRev (f,(_,ts@(x:_:xs))) r = isEmptyNilRule r &&
                                 isConsFun f && isNonterm x && isNonterm (last ts)
  tryRev _ _ = False

isEmptyNilRule (f,(_,ts)) = isNilFun f && null ts

precCat :: Cat -> Int
precCat = snd . analyseCat

precRule :: Rule -> Int
precRule = precCat . valCat

precLevels :: CF -> [Int]
precLevels cf = sort $ nub $ [ precCat c | c <- allCats cf]

precCF :: CF -> Bool
precCF cf = length (precLevels cf) > 1

catCat :: Cat -> Cat
catCat = fst . analyseCat

analyseCat :: Cat -> (Cat,Int)
analyseCat c = if (isList c) then list c else noList c
 where
  list   cat = let (rc,n) = noList (init (tail cat)) in ("[" ++ rc ++ "]",n)
  noList cat = case span isDigit (reverse cat) of
	        ([],c') -> (reverse c', 0)
	        (d,c') ->  (reverse c', read (reverse d))

-- we should actually check that
-- (1) coercions are always between variants
-- (2) no other digits are used

checkRule :: CF -> RuleP -> Either RuleP String
checkRule cf r@((f,_),(cat,rhs))
  | badCoercion    = Right $ "Bad coercion in rule" +++ s
  | badNil         = Right $ "Bad empty list rule" +++ s
  | badOne         = Right $ "Bad one-element list rule" +++ s
  | badCons        = Right $ "Bad list construction rule" +++ s
  | badList        = Right $ "Bad list formation rule" +++ s
  | badSpecial     = Right $ "Bad special category rule" +++ s
  | badTypeName    = Right $ "Bad type name" +++ unwords badtypes +++ "in" +++ s
  | badFunName     = Right $ "Bad constructor name" +++ f +++ "in" +++ s
  | badMissing     = Right $ "No production for" +++ unwords missing ++
                             ", appearing in rule" +++ s
  | otherwise      = Left r
 where
   s  = f ++ "." +++ cat +++ "::=" +++ unwords (map (either id show) rhs) ---
   c  = normCat cat
   cs = [normCat c | Left c <- rhs]
   badCoercion = isCoercion f && not ([c] == cs)
   badNil      = isNilFun f   && not (isList c && null cs)
   badOne      = isOneFun f   && not (isList c && cs == [catOfList c])
   badCons     = isConsFun f  && not (isList c && cs == [catOfList c, c])
   badList     = isList c     &&
                 not (isCoercion f || isNilFun f || isOneFun f || isConsFun f)
   badSpecial  = elem c specialCatsP && not (isCoercion f)

   badMissing  = not (null missing)
   missing     = filter nodef [c | Left c <- rhs]
   nodef t = notElem t defineds
   defineds =
    "#" : map fst (tokenPragmas cf) ++ specialCatsP ++ map valCat (rulesOfCF cf)
   badTypeName = not (null badtypes)
   badtypes = filter isBadType $ cat : [c | Left c <- rhs]
   isBadType c = not (isUpper (head c) || isList c || c == "#")
   badFunName = not (all (\c -> isAlphaNum c || c == '_') f {-isUpper (head f)-}
                       || isCoercion f || isNilFun f || isOneFun f || isConsFun f)

isPositionCat :: CFG f -> Cat -> Bool
isPositionCat cf cat =  or [b | TokenReg name b _ <- pragmasOfCF cf, name == cat]


-- grammar with permutation profile � la GF. AR 22/9/2004

type CFP   = (Exts,[RuleP])
type FunP  = (Fun,Prof)
type RuleP = (FunP, (Cat, [Either Cat String]))

type Prof  = (Fun, [([[Int]],[Int])]) -- the original function name, profile

cf2cfp :: CF -> CFP
cf2cfp (es,rs) = (es, map cf2cfpRule rs)

cf2cfpRule :: Rule -> RuleP
cf2cfpRule (f,(c,its))  = ((f, (f, trivialProf its)),(c,its))

cfp2cf :: CFP -> CF
cfp2cf (es,rs) = (es,[(f,(c,its)) | ((f,_),(c,its)) <- rs])

trivialProf :: [Either Cat String] -> [([[Int]],[Int])]
trivialProf its = [([],[i]) | (i,_) <- zip [0..] [c | Left c <- its]]

rulesOfCFP :: CFP -> [RuleP]
rulesOfCFP = snd

funRuleP :: RuleP -> Fun
funRuleP = fst . snd . fst

ruleGroupsP :: CFP -> [(Cat,[RuleP])]
ruleGroupsP cf = [(c, rulesForCatP cf c) | c <- allCatsP cf]

rulesForCatP :: CFP -> Cat -> [RuleP]
rulesForCatP cf cat = [r | r <- rulesOfCFP cf, isParsable r, valCat r == cat]

allCatsP :: CFP -> [Cat]
allCatsP = nub . map valCat . rulesOfCFP -- no cats w/o production

allEntryPointsP :: CFP -> [Cat]
allEntryPointsP cf = case concat [cats | EntryPoints cats <- pragmasOfCF cf] of
  [] -> allCatsP cf
  cs -> cs
