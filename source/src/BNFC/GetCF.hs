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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | Check LBNF input file and turn it into the 'CF' internal representation.

module BNFC.GetCF
  ( parseCF, parseCFP
  , checkRule, transItem
  ) where

import Control.Arrow (left)
import Control.Monad.Reader (Reader, runReader, MonadReader(..), asks)
import Control.Monad.State (State, evalState, get, modify)
import Control.Monad.Except (MonadError(..))

import Data.Char
import Data.Either  (partitionEithers)
import Data.Functor (($>)) -- ((<&>)) -- only from ghc 8.4
import Data.List    (nub, partition)
import qualified Data.List as List
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
import BNFC.Regex       (nullable, simpReg)
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
                    >>= getCFP opts
                    >>= return . markTokenCategories
  let cf = cfp2cf cfp
  either dieUnlessForce return $ runTypeChecker $ checkDefinitions cf

  -- Some backends do not allow the grammar name to coincide with
  -- one of the category or constructor names.
  let names    = allNames cf
  when (target == TargetJava) $
    case List.find ((lang opts ==) . wpThing) names of
      Nothing -> return ()
      Just px ->
        dieUnlessForce $ unlines
          [ unwords $
            [ "ERROR of backend", show target ++ ":"
            , "the language name"
            , lang opts
            , "conflicts with a name defined in the grammar:"
            ]
          , blendInPosition px
          ]

  -- Some (most) backends do not support layout.
  when (hasLayout cf && target `notElem`
    [ TargetHaskell, TargetHaskellGadt, TargetLatex, TargetPygments, TargetCheck ]) $
      dieUnlessForce $ unwords
        [ "ERROR: the grammar uses layout, which is not supported by backend"
        , show target ++ "."
        ]

  -- Warn or fail if the grammar uses non unique names.
  case filter (not . isDefinedRule) $ filterNonUnique names of
    [] -> return ()
    ns | target `elem` [ TargetCpp , TargetCppNoStl , TargetJava ]
       -> dieUnlessForce $ unlines $ concat
            [ [ "ERROR: names not unique:" ]
            , printNames ns
            , [ "This is an error in the backend " ++ show target ++ "." ]
            ]
       | otherwise
       -> putStrLn $ unlines $ concat
            [ [ "Warning: names not unique:" ]
            , printNames ns
            , [ "This can be an error in some backends." ]
            ]

  -- Warn or fail if the grammar uses names not unique modulo upper/lowercase.
  case filter (not . isDefinedRule) $ duplicatesOn (map toLower . wpThing) names of
    [] -> return ()
    ns | target `elem` [ TargetJava ]
       -> dieUnlessForce $ unlines $ concat
            [ [ "ERROR: names not unique ignoring case:" ]
            , printNames ns
            , [ "This is an error in the backend " ++ show target ++ "."]
            ]
       | otherwise
       -> putStr $ unlines $ concat
            [ [ "Warning: names not unique ignoring case:" ]
            , printNames ns
            , [ "This can be an error in some backends." ]
            ]

  -- Note: the following @() <-@ works around an @Ambiguous type variable@
  () <- when (hasPositionTokens cf && target == TargetCppNoStl) $
      putStrLn $ unwords
        [ "Warning: the backend"
        , show target
        , "ignores the qualifier `position` in token definitions."
        ]

  -- Fail if the grammar uses defined constructors which are not actually defined.
  let definedConstructor = \case
        FunDef x _ _ -> Just x
        _ -> Nothing
  let definedConstructors = Set.fromList $ mapMaybe definedConstructor $ cfgPragmas cf
  let undefinedConstructor x = isDefinedRule x && x `Set.notMember` definedConstructors
  case filter undefinedConstructor $ map funRule $ cfgRules cf of
    [] -> return ()
    xs -> dieUnlessForce $ unlines $ concat
            [ [ "Lower case rule labels need a definition."
              , "ERROR: undefined rule label(s):"
              ]
            , printNames xs
            ]

  -- Print warnings if user defined nullable tokens.
  Fold.mapM_ putStrLn $ checkTokens cf

  -- Check for empty grammar.
  let nRules = length (cfgRules cf)
  -- Note: the match against () is necessary for type class instance resolution.
  when (nRules == 0) $ dieUnlessForce $ "ERROR: the grammar contains no rules."

  -- Check whether one of the parsers could consume at least one token. [#213]
  when (null (usedTokenCats cf) && null (cfTokens cf)) $
    dieUnlessForce $
      "ERROR: the languages defined by this grammar are empty since it mentions no terminals."

  -- Passed the tests: Print the number of rules.
  putStrLn $ show nRules +++ "rules accepted\n"
  return cfp

  where
  runErr = either die return

  dieUnlessForce :: String -> IO ()
  dieUnlessForce msg = do
    hPutStrLn stderr msg
    if force opts then do
      hPutStrLn stderr
        "Ignoring error... (thanks to --force)"
    else do
      hPutStrLn stderr
        "Aborting.  (Use option --force to continue despite errors.)"
      exitFailure

  printNames :: [RString] -> [String]
  printNames = map (("  " ++) . blendInPosition) . List.sortOn lexicoGraphic
    where
    lexicoGraphic (WithPosition pos x) = (pos,x)

die :: String -> IO a
die msg = do
  hPutStrLn stderr msg
  exitFailure

-- | Translate the parsed grammar file into a context-free grammar 'CFP'.
--   Desugars and type-checks.

getCFP :: SharedOptions -> Abs.Grammar -> Err CFP
getCFP opts (Abs.Grammar defs0) = do
    let (defs,inlineDelims)= if cnf opts then (defs0,id) else removeDelims defs0
        (pragma,rules0)    = partitionEithers $ concat $ mapM transDef defs `runTrans` opts
        rules              = inlineDelims rules0
        reservedWords      = nub [t | r <- rules, isParsable r, Right t <- rhsRule r, not $ all isSpace t]
          -- Issue #204: exclude keywords from internal rules
          -- Issue #70: whitespace separators should be treated like "", at least in the parser
        usedCats           = Set.fromList [ c | Rule _ _ rhs _ <- rules, Left c <- rhs ]
        literals           = filter (\ s -> TokenCat s `Set.member` usedCats) $ specialCatsP
        (symbols,keywords) = partition notIdent reservedWords
    sig <- runTypeChecker $ buildSignature $ map (fmap fst) rules
    let
      cf = revs $ CFG
        { cfgPragmas        = pragma
        , cfgLiterals       = literals
        , cfgSymbols        = symbols
        , cfgKeywords       = keywords
        , cfgReversibleCats = []
        , cfgRules          = rules
        , cfgSignature      = sig
        }
    case mapMaybe (checkRule (cfp2cf cf)) rules of
      [] -> return ()
      msgs -> throwError $ unlines msgs
    return cf
  where
    notIdent s       = null s || not (isAlpha (head s)) || any (not . isIdentRest) s
    isIdentRest c    = isAlphaNum c || c == '_'
    revs cfp@CFG{..} =
        cfp { cfgReversibleCats = findAllReversibleCats (cfp2cf cfp) }

-- | This function goes through each rule of a grammar and replace Cat "X" with
-- TokenCat "X" when "X" is a token type.
markTokenCategories :: CFP -> CFP
markTokenCategories cf@CFG{..} = cf { cfgRules = newRules }
  where
    newRules = [ Rule f (fmap mark c) (map (left mark) rhs) internal | Rule f c rhs internal <- cfgRules ]
    tokenCatNames = [ wpThing n | TokenReg n _ _ <- cfgPragmas ] ++ specialCatsP
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
      | c == ListCat (transCat' cat) = [Right open, Left c, Right close]
    inlineDelim _ x = [x]

    inlineDelim' :: Abs.Def -> RuleP -> RuleP
    inlineDelim' d@(Abs.Delimiters cat _ _ _ _) r@(Rule f c rhs internal)
      | wpThing c == ListCat (transCat' cat) = r
      | otherwise = Rule f c (concatMap (inlineDelim d) rhs) internal
    inlineDelim' _ _ = error "Not a delimiters pragma"


    delimToSep (Abs.Delimiters cat _ _ (Abs.SepTerm  s) sz) = Abs.Terminator sz cat s
    delimToSep (Abs.Delimiters cat _ _ (Abs.SepSepar s) sz) = Abs.Separator  sz cat s
    delimToSep (Abs.Delimiters cat _ _  Abs.SepNone     sz) = Abs.Terminator sz cat ""
    delimToSep x = x

-- | Translation monad.
newtype Trans a = Trans { unTrans :: Reader SharedOptions a }
  deriving (Functor, Applicative, Monad, MonadReader SharedOptions)

runTrans :: Trans a -> SharedOptions -> a
runTrans m opts = unTrans m `runReader` opts

transDef :: Abs.Def -> Trans [Either Pragma RuleP]
transDef = \case
    Abs.Rule label cat items  -> do
      f <- transLabel label
      c <- transCat cat
      return $ [ Right $ Rule f c (concatMap transItem items) Parsable ]
    Abs.Internal label cat items  -> do
      f <- transLabel label
      c <- transCat cat
      return $ [ Right $ Rule f c (concatMap transItem items) Internal ]

    Abs.Comment str               -> return [ Left $ CommentS str ]
    Abs.Comments str1 str2        -> return [ Left $ CommentM (str1, str2) ]

    Abs.Token ident reg           -> do x <- transIdent ident; return [Left $ TokenReg x False $ simpReg reg]
    Abs.PosToken ident reg        -> do x <- transIdent ident; return [Left $ TokenReg x True  $ simpReg reg]
    Abs.Entryp cats               -> singleton . Left . EntryPoints <$> mapM transCat cats
    Abs.Separator size ident str  -> map (Right . cf2cfpRule) <$> separatorRules size ident str
    Abs.Terminator size ident str -> map (Right . cf2cfpRule) <$> terminatorRules size ident str
    Abs.Delimiters a b c d e      -> map (Right . cf2cfpRule) <$> delimiterRules a b c d e
    Abs.Coercions ident int       -> map (Right . cf2cfpRule) <$> coercionRules ident int
    Abs.Rules ident strs          -> map (Right . cf2cfpRule) <$> ebnfRules ident strs
    Abs.Layout ss                 -> return [ Left $ Layout ss ]
    Abs.LayoutStop ss             -> return [ Left $ LayoutStop ss]
    Abs.LayoutTop                 -> return [ Left $ LayoutTop ]
    Abs.Function ident xs e       -> do
      f <- transIdent ident
      let xs' = map transArg xs
      return [ Left $ FunDef f xs' $ transExp xs' e ]

delimiterRules :: Abs.Cat -> String -> String -> Abs.Separation -> Abs.MinimumSize -> Trans [Rule]
delimiterRules a0 l r (Abs.SepTerm  "") size = delimiterRules a0 l r Abs.SepNone size
delimiterRules a0 l r (Abs.SepSepar "") size = delimiterRules a0 l r Abs.SepNone size
delimiterRules a0 l r sep size = do
  WithPosition pos a <- transCat a0
  let as = ListCat a
      a' = '@':'@':show a
      c  = '@':'{':show a
      d  = '@':'}':show a
  let wp = WithPosition pos
      rule f c = Rule (wp f) (wp c)
  return $ concat
   -- recognizing a single element
    [ [ rule "(:[])"  (strToCat a')  (Left a : termin) Parsable -- optional terminator/separator

      -- glueing two sublists
      , rule "(++)"   (strToCat a')  [Left (strToCat a'), Left (strToCat a')] Parsable

       -- starting on either side with a delimiter
      , rule "[]"     (strToCat c)   [Right l] Parsable
      , rule (if optFinal then "(:[])" else "[]")
                      (strToCat d)   ([Left a | optFinal] ++ [Right r]) Parsable

       -- gathering chains
      , rule "(++)"   (strToCat c)   [Left (strToCat c), Left (strToCat a')] Parsable
      , rule "(++)"   (strToCat d)   [Left (strToCat a'), Left (strToCat d)] Parsable

       -- finally, put together left and right chains
      , rule "(++)"   as  [Left (strToCat c), Left (strToCat d)] Parsable
      ]
      -- special rule for the empty list if necessary
    , [ rule "[]"     as  [Right l, Right r] Parsable | optEmpty ]
    ]
  where
  -- optionally separated concat. of x and y categories.
  termin = case sep of
     Abs.SepSepar t -> [Right t]
     Abs.SepTerm  t -> [Right t]
     _ -> []
  optFinal = case (sep, size) of
    (Abs.SepSepar _, _             ) -> True
    (Abs.SepTerm _ , Abs.MNonempty ) -> True
    (Abs.SepNone   ,  Abs.MNonempty) -> True
    _ -> False
  optEmpty = case sep of
    Abs.SepSepar _ -> size == Abs.MEmpty
    _ -> False

-- | Translate @separator [nonempty] C "s"@.
--   The position attached to the generated rules is taken from @C@.
--
--   (Ideally, we would take them from the @terminator@ keyword.
--   But BNFC does not deliver position information there.)
--
--   If the user-provided separator consists of white space only,
--   we turn it into a terminator rule to prevent reduce/reduce conflicts.

separatorRules :: Abs.MinimumSize -> Abs.Cat -> String -> Trans [Rule]
separatorRules size c0 s
  | all isSpace s = terminatorRules size c0 s
  | otherwise     = do
      WithPosition pos c <- transCat c0
      let wp = WithPosition pos
      let cs = ListCat c
      let rule x rhs = Rule (wp x) (wp cs) rhs Parsable
      return $ concat
        [ [ rule "[]"    []                         | size == Abs.MEmpty ]
        , [ rule "(:[])" [Left c]                   ]
        , [ rule "(:)"   [Left c, Right s, Left cs] ]
        ]

-- | Translate @terminator [nonempty] C "s"@.
--   The position attached to the generated rules is taken from @C@.
--
--   (Ideally, we would take them from the @terminator@ keyword.
--   But BNFC does not deliver position information there.)

terminatorRules :: Abs.MinimumSize -> Abs.Cat -> String -> Trans [Rule]
terminatorRules size c0 s = do
  WithPosition pos c <- transCat c0
  let wp = WithPosition pos
  let cs = ListCat c
  let rule x rhs = Rule (wp x) (wp cs) rhs Parsable
  return
    [ case size of
      Abs.MNonempty ->
        rule "(:[])" (Left c : term [])
      Abs.MEmpty ->
        rule "[]"    []
    ,   rule "(:)"   (Left c : term [Left cs])
    ]
  where
  term = if null s then id else (Right s :)

coercionRules :: Abs.Identifier -> Integer -> Trans [Rule]
coercionRules c0 n = do
  WithPosition pos c <- transIdent c0
  let wp = WithPosition pos
  let urule x rhs = Rule (wp "_") (wp x) rhs Parsable
  return $ concat
    [ [ urule (Cat c)            [Left (CoercCat c 1)]                ]
    , [ urule (CoercCat c (i-1)) [Left (CoercCat c i)]                | i <- [2..n] ]
    , [ urule (CoercCat c n)     [Right "(", Left (Cat c), Right ")"] ]
    ]

ebnfRules :: Abs.Identifier -> [Abs.RHS] -> Trans [Rule]
ebnfRules (Abs.Identifier ((line, col), c)) rhss = do
  file <- asks lbnfFile
  let wp = WithPosition $ Position file line col
  let rule x rhs = Rule (wp x) (wp $ strToCat c) rhs Parsable
  return
    [ rule (mkFun k its) (concatMap transItem its)
    | (k, Abs.RHS its) <- zip [1 :: Int ..] rhss
    ]
 where
   mkFun k = \case
     [Abs.Terminal s]  -> c' ++ "_" ++ mkName k s
     [Abs.NTerminal n] -> c' ++ identCat (transCat' n)
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
transItem (Abs.Terminal str)  = [ Right w | w <- words str ]
transItem (Abs.NTerminal cat) = [ Left (transCat' cat) ]

transCat' :: Abs.Cat -> Cat
transCat' = \case
    Abs.ListCat cat                      -> ListCat $ transCat' cat
    Abs.IdCat (Abs.Identifier (_pos, c)) -> strToCat c

transCat :: Abs.Cat -> Trans (WithPosition Cat)
transCat = \case
    Abs.ListCat cat                             -> fmap ListCat <$> transCat cat
    Abs.IdCat (Abs.Identifier ((line, col), c)) -> do
      file <- asks lbnfFile
      return $ WithPosition (Position file line col) $ strToCat c

transLabel :: Abs.Label -> Trans (RFun, Prof)
transLabel = \case
    Abs.LabNoP f     -> do g <- transLabelId f; return (g,(g,[])) ---- should be Nothing
    Abs.LabP   f p   -> do g <- transLabelId f; return (g,(g, map transProf p))
    Abs.LabPF  f g p -> (,) <$> transLabelId f <*> do (,map transProf p) <$> transLabelId g
    Abs.LabF   f g   -> (,) <$> transLabelId f <*> do (,[])              <$> transLabelId g
  where
  transLabelId = \case
    Abs.Id id     -> transIdent id
    Abs.Wild      -> return $ noPosition $ "_"
    Abs.ListE     -> return $ noPosition $ "[]"
    Abs.ListCons  -> return $ noPosition $ "(:)"
    Abs.ListOne   -> return $ noPosition $ "(:[])"
  transProf (Abs.ProfIt bss as) =
     ([map fromInteger bs | Abs.Ints bs <- bss], map fromInteger as)

transIdent :: Abs.Identifier -> Trans RString
transIdent (Abs.Identifier ((line, col), str)) = do
  file <- asks lbnfFile
  return $ WithPosition (Position file line col) str

transArg :: Abs.Arg -> String
transArg (Abs.Arg (Abs.Identifier (_pos, x))) = x

transExp
  :: [String] -- ^ Arguments of definition (in scope in expression).
  -> Abs.Exp  -- ^ Expression.
  -> Exp      -- ^ Translated expression.
transExp xs = loop
  where
  loop = \case
    Abs.App x es    -> App (transIdent' x) (map loop es)
    Abs.Var x       -> let x' = transIdent' x in
                       if x' `elem` xs then Var x' else App x' []
    Abs.Cons e1 e2  -> cons e1 (loop e2)
    Abs.List es     -> foldr cons nil es
    Abs.LitInt x    -> LitInt x
    Abs.LitDouble x -> LitDouble x
    Abs.LitChar x   -> LitChar x
    Abs.LitString x -> LitString x
  cons e1 e2 = App "(:)" [loop e1, e2]
  nil        = App "[]" []
  transIdent' (Abs.Identifier (_pos, x)) = x

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


-- we should actually check that
-- (1) coercions are always between variants
-- (2) no other digits are used

checkRule :: CF -> RuleP -> Maybe String
checkRule cf (Rule (f,_) (WithPosition _ cat) rhs _)
  | Cat ('@':_) <- cat = Nothing -- Generated by a pragma; it's a trusted category
  | badCoercion    = failure $ "Bad coercion in rule" +++ s
  | badNil         = failure $ "Bad empty list rule" +++ s
  | badOne         = failure $ "Bad one-element list rule" +++ s
  | badCons        = failure $ "Bad list construction rule" +++ s
  | badList        = failure $ "Bad list formation rule" +++ s
  | badSpecial     = failure $ "Bad special category rule" +++ s
  | badTypeName    = failure $ "Bad type name" +++ unwords (map show badtypes) +++ "in" +++ s
  | badFunName     = failure $ "Bad constructor name" +++ fun +++ "in" +++ s
  | badMissing     = failure $ "no production for" +++ unwords missing ++ ", appearing in rule\n    " ++ s
  | otherwise      = Nothing
 where
   failure = Just . blendInPosition . (f $>)
   fun = wpThing f
   s  = fun ++ "." +++ show cat +++ "::=" +++ unwords (map (either show show) rhs) -- Todo: consider using the show instance of Rule
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
   badFunName = not (all (\c -> isAlphaNum c || c == '_') (wpThing f) {-isUpper (head f)-}
                       || isCoercion f || isNilCons f)


-- | Pre-processor that converts the `rules` macros to regular rules
-- by creating unique function names for them.
-- >>> :{
-- let rules1 = Abs.Rules (Abs.Identifier ((0, 0), "Foo"))
--         [ Abs.RHS [Abs.Terminal "abc"]
--         , Abs.RHS [Abs.NTerminal (Abs.IdCat (Abs.Identifier ((0, 0), "A")))]
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
-- let rules1 = Abs.Rules (Abs.Identifier ((0, 0), "Foo"))
--         [ Abs.RHS [Abs.Terminal "foo", Abs.Terminal "bar"] ]
-- in
-- let rules2 = Abs.Rules (Abs.Identifier ((0, 0), "Foo"))
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
    Abs.Grammar . concat $ mapM expand defs `evalState` []
  where
    expand :: Abs.Def -> State [(String, Int)] [Abs.Def]
    expand = \case
      Abs.Rules ident rhss -> mapM (mkRule ident) rhss
      other                -> return [ other ]

    mkRule :: Abs.Identifier -> Abs.RHS -> State [(String, Int)] Abs.Def
    mkRule ident (Abs.RHS rhs) = do
      fun <- Abs.LabNoP . Abs.Id <$> mkName ident rhs
      return $ Abs.Rule fun (Abs.IdCat ident) rhs

    mkName :: Abs.Identifier -> [Abs.Item] -> State [(String, Int)] Abs.Identifier
    mkName (Abs.Identifier (pos, cat)) = \case

      -- A string that is a valid identifier.
      [ Abs.Terminal s ] | all (\ c -> isAlphaNum c || c == '_') s ->
        return $ Abs.Identifier (pos, cat ++ "_" ++ s)

      -- Same but without double quotes.
      [ Abs.NTerminal (Abs.IdCat (Abs.Identifier (pos', s))) ] ->
        return $ Abs.Identifier (pos', cat ++ s)

      -- Something else that does not immediately give a valid rule name.
      -- Just number!
      _ -> do
        i <- maybe 1 (+1) . lookup cat <$> get
        modify ((cat, i):)
        return $ Abs.Identifier (pos, cat ++ show i)
