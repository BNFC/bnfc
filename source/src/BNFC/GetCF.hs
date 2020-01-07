{-
    BNF Converter: Abstract syntax
    Copyright (C) 2004  Author: Markus Forsberg, Aarne Ranta

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

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Check LBNF input file and turn it into the 'CF' internal representation.

module BNFC.GetCF
  ( parseCF, parseCFP
  , checkRule, transItem
  ) where

import Control.Arrow (left)
import Control.Monad.State (State, evalState, get, modify)
import Control.Monad.Except (MonadError(..))

import Data.Char
import Data.Either  (partitionEithers)
-- import Data.Functor ((<&>)) -- only from ghc 8.4
import Data.List    (nub, partition)
import Data.Maybe   (mapMaybe)

import qualified Data.Foldable as Fold
import qualified Data.Set      as Set

import System.Exit (exitFailure)
import System.IO   (hPutStrLn, stderr)

-- Local imports:

import qualified AbsBNF as Abs
import ParBNF

import BNFC.CF
import BNFC.Options
import BNFC.Regex       (simpReg)
import BNFC.TypeChecker
import BNFC.Utils

type Err = Either String

-- $setup
-- >>> import PrintBNF

-- | Entrypoint.

parseCF :: SharedOptions -> Target -> String -> IO CF
parseCF opts t s = cfp2cf <$> parseCFP opts t s

-- | Entrypoint (profiles).

parseCFP :: SharedOptions -> Target -> String -> IO CFP
parseCFP opts target content = do
  cfp <- runErr $ pGrammar (myLexer content)
                    -- <&> expandRules -- <&> from ghc 8.4
                    >>= return . expandRules
                    >>= getCFP (cnf opts)
                    >>= markTokenCategories
  let cf = cfp2cf cfp
  runErr $ checkDefinitions cf

  -- Some backends do not allow the grammar name to coincide with
  -- one of the category or constructor names.
  let names    = allNames cf
  -- Note: the following @() <-@ works around an @Ambiguous type variable@
  () <- when (target == TargetJava && lang opts `elem` names) $
      die $ unwords $
        [ "ERROR of backend", show target ++ ":"
        , "the language name"
        , lang opts
        , "conflicts with a name defined in the grammar."
        ]

  -- Warn or fail if the grammar uses non unique names.
  case filter (not . isDefinedRule) $ filterNonUnique names of
    [] -> return ()
    ns | target `notElem` [TargetCheck,TargetHaskell,TargetHaskellGadt,TargetOCaml]
       -> die $ unlines $
            [ "ERROR: names not unique: " ++ unwords ns
            , "This is an error in the backend " ++ show target ++ "."
            ]
       | otherwise
       -> putStrLn $ unlines $
            [ "Warning: names not unique: " ++ unwords ns
            , "This can be an error in other backends."
            ]

  -- Warn or fail if the grammar uses names not unique modulo upper/lowercase.
  case filter (not . isDefinedRule) $ duplicatesOn (map toLower) names of
    [] -> return ()
    ns | target `elem` [ TargetC , TargetCpp , TargetCppNoStl , TargetCSharp , TargetJava ]
       -> die $ unlines $
            [ "ERROR: names not unique ignoring case: " ++ unwords ns
            , "This is an error in the backend " ++ show target ++ "."
            ]
       | otherwise
       -> putStrLn $ unlines $
            [ "Warning: names not unique ignoring case: " ++ unwords ns
            , "This can be an error in other backends."
            ]

  -- Fail if the grammar uses defined constructors which are not actually defined.
  let definedConstructor = \case
        FunDef x _ _ -> Just x
        _ -> Nothing
  let definedConstructors = Set.fromList $ mapMaybe definedConstructor $ cfgPragmas cf
  let undefinedConstructor x = isDefinedRule x && x `Set.notMember` definedConstructors
  case filter undefinedConstructor $ map funRule $ cfgRules cf of
    [] -> return ()
    xs -> die $ unlines $
            [ "Lower case rule labels need a definition."
            , "ERROR: undefined rule label(s): " ++ unwords xs
            ]

  -- Print warnings if user defined nullable tokens.
  Fold.mapM_ putStrLn $ checkTokens cf

  -- Print the number of rules
  let nRules = length (cfgRules cf)
  -- Note: the match against () is necessary for type class instance resolution.
  () <- when (nRules == 0) $ die $ "ERROR: the grammar contains no rules."
  putStrLn $ show nRules +++ "rules accepted\n"
  return cfp

  where
    runErr = either die return

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure

{-
    case filter (not . isDefinedRule) $ notUniqueFuns cf of
     [] -> case (badInheritence cf) of
       [] -> return (ret,True)
       xs -> do
        putStrLn "Warning :"
        putStrLn $ "  Bad Label name in Category(s) :" ++ unwords xs
        putStrLn $ "  These categories have more than one Label, yet one of these"
        putStrLn $ "  Labels has the same name as the Category. This will almost"
        putStrLn $ "  certainly cause problems in languages other than Haskell.\n"
        return (ret,True)
     xs -> do
       putStrLn $ "Warning :"
       putStrLn $ "  Non-unique label name(s) : " ++ unwords xs
       putStrLn $ "  There may be problems with the pretty-printer.\n"
       case (badInheritence cf) of
         [] -> return (ret,True)
         xs -> do
          putStrLn $ "Warning :"
          putStrLn $ "  Bad Label name in Category(s) :" ++ unwords xs
          putStrLn $ "  These categories have more than one Label, yet one of these"
          putStrLn $ "  Labels has the same name as the Category. This will almost"
          putStrLn $ "  certainly cause problems in languages other than Haskell.\n"
          return (ret,True)
-}

getCFP :: Bool -> Abs.Grammar -> Err CFP
getCFP cnf (Abs.Grammar defs0) = do
    sig <- buildSignature $ map (fmap fst) rules
    let cf = revs $ CFG pragma literals symbols keywords [] rules sig
    case mapMaybe (checkRule (cfp2cf cf)) rules of
      [] -> return ()
      msgs -> throwError $ unlines msgs
    return cf
  where
    rules              = inlineDelims rules0
    (pragma,rules0)    = partitionEithers $ concatMap transDef defs
    (defs,inlineDelims)= if cnf then (defs0,id) else removeDelims defs0
    literals           = nub
      [ lit | xs <- map rhsRule rules
            , Left (Cat lit) <- xs
            , lit `elem` specialCatsP
      ]
    (symbols,keywords) = partition notIdent reservedWords
    notIdent s         = null s || not (isAlpha (head s)) || any (not . isIdentRest) s
    isIdentRest c      = isAlphaNum c || c == '_' || c == '\''
    reservedWords      = nub [t | r <- rules, isParsable r, Right t <- rhsRule r, not $ all isSpace t]
       -- Issue #204: exclude keywords from internal rules
       -- Issue #70: whitespace separators should be treated like "", at least in the parser
    revs cfp@CFG{..} =
        cfp { cfgReversibleCats = findAllReversibleCats (cfp2cf cfp) }

-- | This function goes through each rule of a grammar and replace Cat "X" with
-- TokenCat "X" when "X" is a token type.
markTokenCategories :: CFP -> Err CFP
markTokenCategories cf@CFG{..} = return $ cf { cfgRules = newRules }
  where
    newRules = [ Rule f (mark c) (map (left mark) rhs) internal | Rule f c rhs internal <- cfgRules ]
    tokenCatNames = [ n | TokenReg n _ _ <- cfgPragmas ] ++ specialCatsP
    mark = toTokenCat tokenCatNames


-- | Change the constructor of categories with the given names from Cat to
-- TokenCat
-- >>> toTokenCat ["A"] (Cat "A") == TokenCat "A"
-- True
-- >>> toTokenCat ["A"] (ListCat (Cat "A")) == ListCat (TokenCat "A")
-- True
-- >>> toTokenCat ["A"] (Cat "B") == Cat "B"
-- True
toTokenCat :: [String] -> Cat -> Cat
toTokenCat ns (Cat a) | a `elem` ns = TokenCat a
toTokenCat ns (ListCat c) = ListCat (toTokenCat ns c)
toTokenCat _ c = c



removeDelims :: [Abs.Def] -> ([Abs.Def], [RuleP] -> [RuleP])
removeDelims xs = (ys ++ map delimToSep ds,
                   foldr (.) id [map (inlineDelim' d) | d <- ds])
  where
    (ds,ys) = partition isDelim xs
    isDelim (Abs.Delimiters{}) = True
    isDelim _ = False

    inlineDelim :: Abs.Def -> Either Cat String ->  [Either Cat String]
    inlineDelim (Abs.Delimiters cat open close _ _) (Left c)
      | c == ListCat (transCat cat) = [Right open, Left c, Right close]
    inlineDelim _ x = [x]

    inlineDelim' :: Abs.Def -> RuleP -> RuleP
    inlineDelim' d@(Abs.Delimiters cat _ _ _ _) r@(Rule f c rhs internal)
      | c == ListCat (transCat cat) = r
      | otherwise = Rule f c (concatMap (inlineDelim d) rhs) internal
    inlineDelim' _ _ = error "Not a delimiters pragma"


    delimToSep (Abs.Delimiters cat _ _ (Abs.SepTerm  s) sz) = Abs.Terminator sz cat s
    delimToSep (Abs.Delimiters cat _ _ (Abs.SepSepar s) sz) = Abs.Separator  sz cat s
    delimToSep (Abs.Delimiters cat _ _  Abs.SepNone     sz) = Abs.Terminator sz cat ""
    delimToSep x = x

transDef :: Abs.Def -> [Either Pragma RuleP]
transDef = \case
 Abs.Rule label cat items ->
   [Right $ Rule (transLabel label) (transCat cat) (concatMap transItem items) Parsable]
 Abs.Comment str               -> [Left $ CommentS str]
 Abs.Comments str0 str         -> [Left $ CommentM (str0,str)]
 Abs.Token ident reg           -> [Left $ TokenReg (transIdent ident) False $ simpReg reg]
 Abs.PosToken ident reg        -> [Left $ TokenReg (transIdent ident) True  $ simpReg reg]
 Abs.Entryp idents             -> [Left $ EntryPoints (map (strToCat .transIdent) idents)]
 Abs.Internal label cat items  ->
   [Right $ Rule (transLabel label) (transCat cat) (concatMap transItem items) Internal]
 Abs.Separator size ident str -> map  (Right . cf2cfpRule) $ separatorRules size ident str
 Abs.Terminator size ident str -> map  (Right . cf2cfpRule) $ terminatorRules size ident str
 Abs.Delimiters a b c d e -> map  (Right . cf2cfpRule) $ delimiterRules a b c d e
 Abs.Coercions ident int -> map  (Right . cf2cfpRule) $ coercionRules ident int
 Abs.Rules ident strs -> map (Right . cf2cfpRule) $ ebnfRules ident strs
 Abs.Layout ss      -> [Left $ Layout ss]
 Abs.LayoutStop ss  -> [Left $ LayoutStop ss]
 Abs.LayoutTop      -> [Left LayoutTop]
 Abs.Function f xs e -> [ Left $ FunDef (transIdent f) xs' (transExp xs' e)
                        | let xs' = map transArg xs ]

delimiterRules :: Abs.Cat -> String -> String -> Abs.Separation -> Abs.MinimumSize -> [Rule]
delimiterRules a0 l r (Abs.SepTerm  "") size = delimiterRules a0 l r Abs.SepNone size
delimiterRules a0 l r (Abs.SepSepar "") size = delimiterRules a0 l r Abs.SepNone size
delimiterRules a0 l r sep size =
   -- recognizing a single element
  [ Rule "(:[])"  (strToCat a')  (Left a : termin) Parsable -- optional terminator/separator

  -- glueing two sublists
  , Rule "(++)"   (strToCat a')  [Left (strToCat a'), Left (strToCat a')] Parsable

   -- starting on either side with a delimiter
  , Rule "[]"     (strToCat c)   [Right l] Parsable
  , Rule (if optFinal then "(:[])" else
                         "[]")
                (strToCat d)   ([Left a | optFinal] ++ [Right r]) Parsable

   -- gathering chains
  , Rule "(++)"   (strToCat c)   [Left (strToCat c), Left (strToCat a')] Parsable
  , Rule "(++)"   (strToCat d)   [Left (strToCat a'), Left (strToCat d)] Parsable

   -- finally, put together left and right chains
  , Rule "(++)"   as  [Left (strToCat c),Left (strToCat d)] Parsable
  ] ++
  -- special rule for the empty list if necessary
  [ Rule "[]"     as  [Right l,Right r] Parsable | optEmpty ]
 where a = transCat a0
       as = ListCat a
       a' = '@':'@':show a
       c  = '@':'{':show a
       d  = '@':'}':show a
       -- optionally separated concat. of x and y categories.
       termin = case sep of
                  Abs.SepSepar t -> [Right t]
                  Abs.SepTerm  t -> [Right t]
                  _ -> []
       optFinal = case (sep,size) of
         (Abs.SepSepar _,_) -> True
         (Abs.SepTerm _,Abs.MNonempty) -> True
         (Abs.SepNone,Abs.MNonempty) -> True
         _ -> False
       optEmpty = case sep of
         Abs.SepSepar _ -> size == Abs.MEmpty
         _ -> False

-- If the user-provided separator consists of white space only,
-- we turn it into a terminator rule to prevent reduce/reduce conflicts.
separatorRules :: Abs.MinimumSize -> Abs.Cat -> String -> [Rule]
separatorRules size c s = if all isSpace s then terminatorRules size c s else ifEmpty [
  Rule "(:[])" cs [Left c'] Parsable,
  Rule "(:)"   cs [Left c', Right s, Left cs] Parsable
  ]
 where
   c' = transCat c
   cs = ListCat c'
   ifEmpty rs = if size == Abs.MNonempty
                then rs
                else Rule "[]" cs [] Parsable : rs

terminatorRules :: Abs.MinimumSize -> Abs.Cat -> String -> [Rule]
terminatorRules size c s = [
  ifEmpty,
  Rule "(:)" cs (Left c' : s' [Left cs]) Parsable
  ]
 where
   c' = transCat c
   cs = ListCat c'
   s' its = if null s then its else Right s : its
   ifEmpty = if size == Abs.MNonempty
                then Rule "(:[])" cs (Left c' : if null s then [] else [Right s]) Parsable
                else Rule "[]" cs [] Parsable

coercionRules :: Abs.Identifier -> Integer -> [Rule]
coercionRules (Abs.Identifier c) n = concat
  [ [ Rule "_" (Cat c)            [Left (CoercCat c 1)] Parsable
    ]
  , [ Rule "_" (CoercCat c (i-1)) [Left (CoercCat c i)] Parsable
    | i <- [2..n]
    ]
  , [ Rule "_" (CoercCat c n)     [Right "(", Left (Cat c), Right ")"] Parsable
    ]
  ]

ebnfRules :: Abs.Identifier -> [Abs.RHS] -> [Rule]
ebnfRules (Abs.Identifier c) rhss =
  [Rule (mkFun k its) (strToCat c) (concatMap transItem its) Parsable
     | (k, Abs.RHS its) <- zip [1 :: Int ..] rhss]
 where
   mkFun k i = case i of
     [Abs.Terminal s]  -> c' ++ "_" ++ mkName k s
     [Abs.NTerminal n] -> c' ++ identCat (transCat n)
     _ -> c' ++ "_" ++ show k
   c' = c --- normCat c
   mkName k s = if all (\c -> isAlphaNum c || elem c ("_'" :: String)) s
                   then s else show k

-- | Translate a rule item (terminal or non terminal)
-- It also sanitizes the terminals a bit by skipping empty terminals
-- or splitting multiwords terminals.
-- This means that the following rule
--   Foo. S ::= "foo bar" ""
-- is equivalent to
--   Foo. S ::= "foo" "bar"
transItem :: Abs.Item -> [Either Cat String]
transItem (Abs.Terminal str) = [Right w | w <- words str]
transItem (Abs.NTerminal cat) = [Left (transCat cat)]

transCat :: Abs.Cat -> Cat
transCat x = case x of
 Abs.ListCat cat  -> ListCat (transCat cat)
 Abs.IdCat (Abs.Identifier c)     -> strToCat c

transLabel :: Abs.Label -> (Fun,Prof)
transLabel y = case y of
   Abs.LabNoP f     -> let g = transLabelId f in (g,(g,[])) ---- should be Nothing
   Abs.LabP   f p   -> let g = transLabelId f in (g,(g, map transProf p))
   Abs.LabPF  f g p -> (transLabelId f,(transLabelId g, map transProf p))
   Abs.LabF   f g   -> (transLabelId f,(transLabelId g, []))
 where
   transLabelId x = case x of
     Abs.Id id     -> transIdent id
     Abs.Wild      -> "_"
     Abs.ListE     -> "[]"
     Abs.ListCons  -> "(:)"
     Abs.ListOne   -> "(:[])"
   transProf (Abs.ProfIt bss as) =
     ([map fromInteger bs | Abs.Ints bs <- bss], map fromInteger as)

transIdent :: Abs.Identifier -> String
transIdent = \case
 Abs.Identifier str  -> str

transArg :: Abs.Arg -> String
transArg (Abs.Arg x) = transIdent x

transExp
  :: [String] -- ^ Arguments of definition (in scope in expression).
  -> Abs.Exp  -- ^ Expression.
  -> Exp      -- ^ Translated expression.
transExp xs = loop
  where
  loop = \case
    Abs.App x es    -> App (transIdent x) (map loop es)
    Abs.Var x       -> let x' = transIdent x in
                       if x' `elem` xs then Var x' else App x' []
    Abs.Cons e1 e2  -> cons e1 (loop e2)
    Abs.List es     -> foldr cons nil es
    Abs.LitInt x    -> LitInt x
    Abs.LitDouble x -> LitDouble x
    Abs.LitChar x   -> LitChar x
    Abs.LitString x -> LitString x
  cons e1 e2 = App "(:)" [loop e1, e2]
  nil        = App "[]" []

--------------------------------------------------------------------------------

-- | Check if any of the user-defined terminal categories is nullable.
checkTokens :: CFG f -> Maybe String
checkTokens cf
  | null ns   = Nothing
  | otherwise = Just $ unlines
      [ "Warning : "  -- TODO: change to error in a future version
      , "  The following tokens accept the empty string: "
      , "    " ++ unwords ns
      , "  This is error-prone and will not be supported in the future."
      ]
  where
    ns = map (show . fst) . filter (nullable . snd) $ tokenPragmas cf

-- | Check if a regular expression is nullable (accepts the empty string)
nullable :: Abs.Reg -> Bool
nullable r =
    case r of
      Abs.RSeq r1 r2   -> nullable r1 && nullable r2
      Abs.RAlt r1 r2   -> nullable r1 || nullable r2
      Abs.RMinus r1 r2 -> nullable r1 && not (nullable r2)
      Abs.RStar _      -> True
      Abs.RPlus r1     -> nullable r1
      Abs.ROpt _       -> True
      Abs.REps         -> True
      Abs.RChar _      -> False
      Abs.RAlts _      -> False
      Abs.RSeqs s      -> null s
      Abs.RDigit       -> False
      Abs.RLetter      -> False
      Abs.RUpper       -> False
      Abs.RLower       -> False
      Abs.RAny         -> False


-- we should actually check that
-- (1) coercions are always between variants
-- (2) no other digits are used

checkRule :: CF -> RuleP -> Maybe String
checkRule _ (Rule _ (Cat ('@':_)) _ _) = Nothing -- Generated by a pragma; it's a trusted category
checkRule cf (Rule (f,_) cat rhs _)
  | badCoercion    = Just $ "Bad coercion in rule" +++ s
  | badNil         = Just $ "Bad empty list rule" +++ s
  | badOne         = Just $ "Bad one-element list rule" +++ s
  | badCons        = Just $ "Bad list construction rule" +++ s
  | badList        = Just $ "Bad list formation rule" +++ s
  | badSpecial     = Just $ "Bad special category rule" +++ s
  | badTypeName    = Just $ "Bad type name" +++ unwords (map show badtypes) +++ "in" +++ s
  | badFunName     = Just $ "Bad constructor name" +++ f +++ "in" +++ s
  | badMissing     = Just $ "no production for" +++ unwords missing ++
                             ", appearing in rule\n    " ++ s
  | otherwise      = Nothing
 where
   s  = f ++ "." +++ show cat +++ "::=" +++ unwords (map (either show show) rhs) -- Todo: consider using the show instance of Rule
   c  = normCat cat
   cs = [normCat c | Left c <- rhs]
   badCoercion = isCoercion f && [c] /= cs
   badNil      = isNilFun f   && not (isList c && null cs)
   badOne      = isOneFun f   && not (isList c && cs == [catOfList c])
   badCons     = isConsFun f  && not (isList c && cs == [catOfList c, c])
   badList     = isList c     &&
                 not (isCoercion f || isNilCons f)
   badSpecial  = elem c [ Cat x | x <- specialCatsP] && not (isCoercion f)

   badMissing  = not (null missing)
   missing     = filter (`notElem` defineds) [show c | Left c <- rhs]
   defineds = tokenNames cf ++ specialCatsP ++ map (show . valCat) (cfgRules cf)
   badTypeName = not (null badtypes)
   badtypes = filter isBadType $ cat : [c | Left c <- rhs]
   isBadType (ListCat c) = isBadType c
   isBadType (CoercCat c _) = isBadCatName c
   isBadType (Cat s) = isBadCatName s
   isBadType (TokenCat s) = isBadCatName s
   isBadCatName s = not $ isUpper (head s) || (head s == '@')
   badFunName = not (all (\c -> isAlphaNum c || c == '_') f {-isUpper (head f)-}
                       || isCoercion f || isNilCons f)


-- | Pre-processor that converts the `rules` macros to regular rules
-- by creating unique function names for them.
-- >>> :{
-- let rules1 = Abs.Rules (Abs.Identifier "Foo")
--         [ Abs.RHS [Abs.Terminal "abc"]
--         , Abs.RHS [Abs.NTerminal (Abs.IdCat (Abs.Identifier "A"))]
--         , Abs.RHS [Abs.Terminal "foo", Abs.Terminal "bar"]
--         , Abs.RHS [Abs.Terminal "++"]
--         ]
-- in
-- let tree = expandRules (Abs.Grammar [rules1])
-- in putStrLn (printTree tree)
-- :}
-- Foo_abc . Foo ::= "abc";
-- FooA . Foo ::= A;
-- Foo1 . Foo ::= "foo" "bar";
-- Foo2 . Foo ::= "++"
--
-- Note that if there are two `rules` macro with the same category, the
-- generated names should be uniques:
-- >>> :{
-- let rules1 = Abs.Rules (Abs.Identifier "Foo")
--         [ Abs.RHS [Abs.Terminal "foo", Abs.Terminal "bar"] ]
-- in
-- let rules2 = Abs.Rules (Abs.Identifier "Foo")
--         [ Abs.RHS [Abs.Terminal "foo", Abs.Terminal "foo"] ]
-- in
-- let tree = expandRules (Abs.Grammar [rules1, rules2])
-- in putStrLn (printTree tree)
-- :}
-- Foo1 . Foo ::= "foo" "bar";
-- Foo2 . Foo ::= "foo" "foo"
--
-- This is using a State monad to remember the last used index for a category.
expandRules :: Abs.Grammar -> Abs.Grammar
expandRules (Abs.Grammar defs) =
    Abs.Grammar (concat (evalState (mapM expand defs) []))
  where
    expand :: Abs.Def -> State [(String, Int)] [Abs.Def]
    expand (Abs.Rules ident rhss) = mapM (mkRule ident) rhss
    expand other = return [other]

    mkRule :: Abs.Identifier -> Abs.RHS -> State [(String, Int)] Abs.Def
    mkRule ident (Abs.RHS rhs) = do
      fun <- Abs.LabNoP . Abs.Id . Abs.Identifier <$> mkName ident rhs
      return (Abs.Rule fun (Abs.IdCat ident) rhs)

    mkName :: Abs.Identifier -> [Abs.Item] -> State [(String, Int)] String
    mkName (Abs.Identifier cat) [Abs.Terminal s]
      | all (\c -> isAlphaNum c || elem c ("_'" :: String)) s =
        return (cat ++ "_" ++ s)
    mkName (Abs.Identifier cat) [Abs.NTerminal (Abs.IdCat (Abs.Identifier s))] =
        return (cat ++ s)
    mkName (Abs.Identifier cat) _ = do
        i <- maybe 1 (+1) . lookup cat <$> get
        modify ((cat, i):)
        return (cat ++ show i)
