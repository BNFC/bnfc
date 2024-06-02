{-
    BNF Converter: Abstract syntax
    Copyright (C) 2004  Author: Markus Forsberg, Aarne Ranta

-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}  -- for type equality ~
{-# LANGUAGE NoMonoLocalBinds #-} -- counteract TypeFamilies

-- | Check LBNF input file and turn it into the 'CF' internal representation.

module BNFC.GetCF
  ( parseCF
  , checkRule, transItem
  ) where

import Control.Arrow (left)
import Control.Monad.Reader (ReaderT, runReaderT, MonadReader(..), asks)
import Control.Monad.State (State, evalState, get, modify)
import Control.Monad.Except (MonadError(..))

import Data.Char
import Data.Either  (partitionEithers)
import Data.Functor (($>)) -- ((<&>)) -- only from ghc 8.4
import Data.List    (nub, partition)
import Data.List.NonEmpty (pattern (:|))
import qualified Data.List as List
import qualified Data.List.NonEmpty as List1
import Data.Maybe

import Data.Set (Set)
import qualified Data.Foldable as Fold
import qualified Data.Set      as Set
import qualified Data.Map      as Map

import System.Exit (exitFailure)
import System.IO   (hPutStrLn, stderr)

-- Local imports:

import qualified BNFC.Abs as Abs
import BNFC.Abs (Reg(RAlts))
import BNFC.Par

import BNFC.CF
import BNFC.Check.EmptyTypes
import BNFC.Options
import BNFC.PrettyPrint
import BNFC.Regex       (nullable, simpReg)
import BNFC.TypeChecker
import BNFC.Utils

type Err = Either String

-- $setup
-- >>> import BNFC.Print

-- | Entrypoint.

parseCF :: SharedOptions -> Target -> String -> IO CF
parseCF opts target content = do
  cf <- runErr $ pGrammar (myLexer content)
                    -- <&> expandRules -- <&> from ghc 8.4
                    >>= return . expandRules
                    >>= getCF opts
                    >>= return . markTokenCategories

  -- Construct the typing information in 'define' expressions.
  cf <- either die return $ runTypeChecker $ checkDefinitions cf

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
  let (layoutTop, layoutKeywords, _) = layoutPragmas cf
  let lay = isJust layoutTop || not (null layoutKeywords)
  when (lay && target `notElem`
    [ TargetHaskell, TargetHaskellGadt, TargetLatex, TargetPygments, TargetCheck ]) $
      dieUnlessForce $ unwords
        [ "ERROR: the grammar uses layout, which is not supported by backend"
        , show target ++ "."
        ]

  -- A grammar that uses layout needs to contain symbols { } ;
  let symbols = cfgSymbols cf
      layoutSymbols = concat [ [";"], unless (null layoutKeywords) ["{", "}"] ]
      missingLayoutSymbols = filter (`notElem` symbols) layoutSymbols
  when (lay && not (null missingLayoutSymbols)) $
      dieUnlessForce $ unwords $
        "ERROR: the grammar uses layout, but does not mention"
        : map show missingLayoutSymbols

  -- Token types that end in a numeral confuse BNFC, because of CoerceCat.
  let userTokenTypes = [ rx | TokenReg rx _ _ <- cfgPragmas cf ]
  case filter (isJust . hasNumericSuffix . wpThing) userTokenTypes of
    []  -> return ()
    rxs -> dieUnlessForce $ unlines $ concat
             [ [ "ERROR: illegal token names:" ]
             , printNames rxs
             , [ "Token names may not end with a number---to avoid confusion with coercion categories." ]
             ]

  -- Fail if grammar defines a @token@ twice.
  case duplicatesOn wpThing userTokenTypes of
    [] -> return ()
    gs -> dieUnlessForce $ unlines $ concat
             [ [ "ERROR: duplicate token definitions:" ]
             , map printDuplicateTokenDefs gs
             ]
      where
      printDuplicateTokenDefs (rx :| rxs) = concat $
         [ concat [ "  ", wpThing rx, " at " ]
         , unwords $ map (prettyPosition . wpPosition) (rx : rxs)
         ]

  -- Fail if token name conflicts with category name.
  let userTokenNames = Map.fromList $ map (\ rx -> (wpThing rx, rx)) userTokenTypes
  case mapMaybe (\ rx -> (rx,) <$> Map.lookup (wpThing rx) userTokenNames) (allCatsIdNorm cf) of
    [] -> return ()
    ns -> dieUnlessForce $ unlines $ concat
             [ [ "ERROR: these token definitions conflict with non-terminals:" ]
             , map (\ (rx, rp) -> "  " ++ blendInPosition rp ++ " conflicts with " ++ blendInPosition rx) ns
             ]

  -- Warn or fail if the grammar uses non unique names.
  let nonUniqueNames = filter (not . isDefinedRule) $ filterNonUnique names
  case nonUniqueNames of
    [] -> return ()
    ns | target `elem` [ TargetC, TargetCpp , TargetCppNoStl , TargetJava ]
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
  case nub
    . filter (`notElem` nonUniqueNames)
    . concatMap List1.toList
    . duplicatesOn (map toLower . wpThing)
    . filter (not . isDefinedRule)
    $ names
    of
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
  let definedConstructors = Set.fromList $ map defName $ definitions cf
  let undefinedConstructor x = isDefinedRule x && x `Set.notMember` definedConstructors
  case filter undefinedConstructor $ map funRule $ cfgRules cf of
    [] -> return ()
    xs -> dieUnlessForce $ unlines $ concat
            [ [ "Lower case rule labels need a definition."
              , "ERROR: undefined rule label(s):"
              ]
            , printNames xs
            ]

  -- Print errors for empty comment deliminters
  unlessNull (checkComments cf) $ \ errs -> do
    dieUnlessForce $ unlines errs

  -- Print warnings if user defined nullable tokens.
  Fold.mapM_ dieUnlessForce $ checkTokens cf

  -- Check for empty grammar.
  let nRules = length (cfgRules cf)
  -- Note: the match against () is necessary for type class instance resolution.
  when (nRules == 0) $ dieUnlessForce $ "ERROR: the grammar contains no rules."

  -- Check whether one of the parsers could consume at least one token. [#213]
  when (null (usedTokenCats cf) && null (cfTokens cf)) $
    dieUnlessForce $
      "ERROR: the languages defined by this grammar are empty since it mentions no terminals."

  unlessNull (emptyData $ cfgRules cf) $ \ pcs -> do
    dieUnlessForce $ unlines $ concat
      [ [ "ERROR: the following categories have empty abstract syntax:" ]
      , printNames $ map (fmap catToStr) pcs
      ]

  -- Passed the tests: Print the number of rules.
  putStrLn $ show nRules +++ "rules accepted\n"
  return cf

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

  -- | All token categories used in the grammar.
  --   Includes internal rules.
  usedTokenCats :: CFG f -> [TokenCat]
  usedTokenCats cf = [ c | Rule _ _ rhs _ <- cfgRules cf, Left (TokenCat c) <- rhs ]

-- | Print vertical list of names with position sorted by position.
printNames :: [RString] -> [String]
printNames = map (("  " ++) . blendInPosition) . List.sortOn lexicoGraphic
  where
  lexicoGraphic (WithPosition pos x) = (pos,x)

die :: String -> IO a
die msg = do
  hPutStrLn stderr msg
  exitFailure

-- | Translate the parsed grammar file into a context-free grammar 'CF'.
--   Desugars and type-checks.

getCF :: SharedOptions -> Abs.Grammar -> Err CF
getCF opts (Abs.Grammar defs) = do
    (pragma, rules) <- partitionEithers . concat <$> mapM transDef defs `runTrans` opts
    let reservedWords      = nub [ t | r <- rules, isParsable r, Right t <- rhsRule r, not $ all isSpace t ]
          -- Issue #204: exclude keywords from internal rules
          -- Issue #70: whitespace separators should be treated like "", at least in the parser
        usedCats           = Set.fromList [ c | Rule _ _ rhs _ <- rules, Left c <- rhs ]
        -- literals = used builtin token cats (Integer, String, ...)
        literals           = filter (\ s -> TokenCat s `Set.member` usedCats) $ specialCatsP
        (symbols,keywords) = partition notIdent reservedWords
    sig <- runTypeChecker $ buildSignature rules
    let
      cf = revs $ CFG
        { cfgPragmas        = pragma
        , cfgUsedCats       = usedCats
        , cfgLiterals       = literals
        , cfgSymbols        = symbols
        , cfgKeywords       = keywords
        , cfgReversibleCats = []
        , cfgRules          = rules
        , cfgSignature      = sig
        }
    case mapMaybe (checkRule cf) rules of
      [] -> return ()
      msgs -> throwError $ unlines msgs
    return cf
  where
    notIdent s       = null s || not (isAlpha (head s)) || any (not . isIdentRest) s
    isIdentRest c    = isAlphaNum c || c == '_' || c == '\''
    revs cf =
        cf{ cfgReversibleCats = findAllReversibleCats cf }

-- | This function goes through each rule of a grammar and replace Cat "X" with
-- TokenCat "X" when "X" is a token type.
markTokenCategories :: CF -> CF
markTokenCategories cf = fixTokenCats tokenCatNames cf
  where
  tokenCatNames = [ wpThing n | TokenReg n _ _ <- cfgPragmas cf ] ++ specialCatsP

class FixTokenCats a where
  fixTokenCats :: [TokenCat] -> a -> a

  default fixTokenCats :: (Functor t, FixTokenCats b, t b ~ a) => [TokenCat] -> a -> a
  fixTokenCats = fmap . fixTokenCats

instance FixTokenCats a => FixTokenCats [a]
instance FixTokenCats a => FixTokenCats (WithPosition a)

instance (FixTokenCats a, Ord a) => FixTokenCats (Set a) where
  fixTokenCats = Set.map . fixTokenCats

-- | Change the constructor of categories with the given names from Cat to
-- TokenCat
-- >>> fixTokenCats ["A"] (Cat "A") == TokenCat "A"
-- True
-- >>> fixTokenCats ["A"] (ListCat (Cat "A")) == ListCat (TokenCat "A")
-- True
-- >>> fixTokenCats ["A"] (Cat "B") == Cat "B"
-- True

instance FixTokenCats Cat where
  fixTokenCats ns = \case
    Cat a | a `elem` ns -> TokenCat a
    ListCat c           -> ListCat $ fixTokenCats ns c
    c -> c

instance FixTokenCats (Either Cat String) where
  fixTokenCats = left . fixTokenCats

instance FixTokenCats (Rul f) where
  fixTokenCats ns (Rule f c rhs internal) =
    Rule f (fixTokenCats ns c) (fixTokenCats ns rhs) internal

instance FixTokenCats Pragma where
  fixTokenCats ns = \case
    EntryPoints eps -> EntryPoints $ fixTokenCats ns eps
    p -> p

instance FixTokenCats (CFG f) where
  fixTokenCats ns cf@CFG{..} = cf
    { cfgPragmas  = fixTokenCats ns cfgPragmas
    , cfgUsedCats = fixTokenCats ns cfgUsedCats
    , cfgRules    = fixTokenCats ns cfgRules
    }

-- | Translation monad.
newtype Trans a = Trans { unTrans :: ReaderT SharedOptions Err a }
  deriving (Functor, Applicative, Monad, MonadReader SharedOptions, MonadError String)

runTrans :: Trans a -> SharedOptions -> Err a
runTrans m opts = unTrans m `runReaderT` opts

transDef :: Abs.Def -> Trans [Either Pragma Rule]
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
    Abs.Separator size ident str  -> map Right <$> separatorRules size ident str
    Abs.Terminator size ident str -> map Right <$> terminatorRules size ident str
    Abs.Delimiters cat _ _ _ _    -> do
      WithPosition pos _ <- transCat cat
      throwError $ blendInPosition $ WithPosition pos $
        "The delimiters pragma " ++ removedIn290
    Abs.Coercions ident int       -> map Right <$> coercionRules ident int
    Abs.Rules ident strs          -> map Right <$> ebnfRules ident strs
    Abs.Layout ss                 -> return [ Left $ Layout $ map (,Delimiters ";" "{" "}") ss ]
    Abs.LayoutStop ss             -> return [ Left $ LayoutStop ss]
    Abs.LayoutTop                 -> return [ Left $ LayoutTop ";" ]
    Abs.Function ident xs e       -> do
      f <- transIdent ident
      let xs' = map transArg xs
      return [ Left $ FunDef $ Define f xs' (transExp (map fst xs') e) dummyBase ]

-- | Translate @separator [nonempty] C "s"@.
--   The position attached to the generated rules is taken from @C@.
--
--   (Ideally, we would take them from the @separator@ keyword.
--   But BNFC does not deliver position information there.)
--
--   If the user-provided separator consists of white space only,
--   we turn it into a terminator rule to prevent reduce/reduce conflicts.

separatorRules :: Abs.MinimumSize -> Abs.Cat -> String -> Trans [Rule]
separatorRules size c0 s
  | all isSpace s = terminatorRules size c0 s
  | otherwise     = do
      WithPosition pos c <- transCat c0
      let cs = ListCat c
      let rule :: String -> SentForm -> Rule
          rule x rhs = Rule (WithPosition pos x) (WithPosition pos cs) rhs Parsable
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

-- | Expansion of the @coercion@ pragma.

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

-- | Expansion of the @rules@ pragma.

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
--
-- >  Foo. S ::= "foo bar" ""
--
-- is equivalent to
--
-- >  Foo. S ::= "foo" "bar"

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

transLabel :: Abs.Label -> Trans RFun
transLabel = \case
    Abs.Id id     -> transIdent id
    Abs.Wild      -> return $ noPosition $ "_"
    Abs.ListE     -> return $ noPosition $ "[]"
    Abs.ListCons  -> return $ noPosition $ "(:)"
    Abs.ListOne   -> return $ noPosition $ "(:[])"

transIdent :: Abs.Identifier -> Trans RString
transIdent (Abs.Identifier ((line, col), str)) = do
  file <- asks lbnfFile
  return $ WithPosition (Position file line col) str

transArg :: Abs.Arg -> (String, Base)
transArg (Abs.Arg (Abs.Identifier (_pos, x))) = (x, dummyBase)

transExp
  :: [String] -- ^ Arguments of definition (in scope in expression).
  -> Abs.Exp  -- ^ Expression.
  -> Exp      -- ^ Translated expression.
transExp xs = loop
  where
  loop = \case
    Abs.App x es    -> App (transIdent' x) dummyType (map loop es)
    Abs.Var x       -> let x' = transIdent' x in
                       if x' `elem` xs then Var x' else App x' dummyType []
    Abs.Cons e1 e2  -> cons e1 (loop e2)
    Abs.List es     -> foldr cons nil es
    Abs.LitInt x    -> LitInt x
    Abs.LitDouble x -> LitDouble x
    Abs.LitChar x   -> LitChar x
    Abs.LitString x -> LitString x
  cons e1 e2 = App "(:)" dummyType [loop e1, e2]
  nil        = App "[]"  dummyType []
  transIdent' (Abs.Identifier (_pos, x)) = x

--------------------------------------------------------------------------------

-- | Check if any comment delimiter is null.
checkComments :: CFG f -> [String]  -- ^ List of errors.
checkComments cf = concat
  [ [ "Empty line comment delimiter."        | CommentS ""      <- prags ]
  , [ "Empty block comment start delimiter." | CommentM ("", _) <- prags ]
  , [ "Empty block comment end delimiter."   | CommentM (_, "") <- prags ]
  ]
  where
  prags = cfgPragmas cf

-- | Check if any of the user-defined terminal categories is nullable or empty.
checkTokens :: CFG f -> Maybe String
checkTokens cf =
  case catMaybes [ checkTokensEmpty cf, checkTokensNullable cf ] of
    [] -> Nothing
    ss -> Just $ concat ss

-- | Check if any of the user-defined terminal categories is nullable.
checkTokensNullable :: CFG f -> Maybe String
checkTokensNullable cf
  | null pxs  = Nothing
  | otherwise = Just $ unlines $ concat
      [ [ "ERROR: The following tokens accept the empty string:" ]
      , printNames pxs
      ]
  where
    pxs = [ px | TokenReg px _ regex <- cfgPragmas cf, nullable regex ]

-- | Check if any of the user-defined terminal categories is nullable.
checkTokensEmpty :: CFG f -> Maybe String
checkTokensEmpty cf
  | null pxs  = Nothing
  | otherwise = Just $ unlines $ concat
      [ [ "ERROR: The following tokens accept nothing:" ]
      , printNames pxs
      ]
  where
    -- The regular expression is already simplified, so we match against 0 directly.
    pxs = [ px | TokenReg px _ (RAlts "") <- cfgPragmas cf ]


-- we should actually check that
-- (1) coercions are always between variants
-- (2) no other digits are used

checkRule :: CF -> Rule -> Maybe String
checkRule cf r@(Rule f (WithPosition _ cat) rhs _)
  | Cat ('@':_) <- cat = Nothing -- Generated by a pragma; it's a trusted category
  | badCoercion = stdFail txtCoercion "Bad coercion in rule"
  | badNil      = stdFail txtNil      "Bad empty list rule"
  | badOne      = stdFail txtOne      "Bad one-element list rule"
  | badCons     = stdFail txtCons     "Bad list construction rule"
  | badList     = stdFail txtList     "Bad list formation rule"
  | badSpecial  = failure $ "Bad special category rule" +++ s
  | badTypeName = failure $ "Bad type name" +++ unwords (map prettyShow badTypes) +++ "in" +++ s
  | badFunName  = failure $ "Bad constructor name" +++ fun +++ "in" +++ s
  | badMissing  = failure $ "no production for" +++ unwords missing ++ ", appearing in rule\n    " ++ s
  | otherwise   = Nothing
 where
   failure = Just . blendInPosition . (f $>)
   stdFail txt err = failure $ unlines [ err ++ ":", "  " ++ s, txt ]

   fun = wpThing f
   s  = prettyShow r
   c  = normCat cat                  -- lhs cat without the coercion number
   cs = [normCat c | Left c <- rhs]  -- rhs cats without the coercion numbers

   badCoercion = isCoercion f && cs /= [c]   -- the single rhs cat needs to match the lhs cat
   txtCoercion = "In a coercion (label _), category on the left of ::= needs to be the single category on the right."

   badNil = isNilFun f   && not (isList c && null cs)
   txtNil = "In a nil rule (label []), the category on the left of ::= needs to be a list category [C] and no categories are allowed on the right."

   badOne = isOneFun f   && not (isList c && cs == [catOfList c])
   txtOne = "In a singleton rule (label (:[])), the category on the left of ::= needs to be a list category [C], and C must be the sole categories on the right."

   badCons = isConsFun f  && not (isList c && cs == [catOfList c, c])
   txtCons = "In a cons rule (label (:)), the category on the left of ::= needs to be a list category [C], and C and [C] (in this order) must be the sole categories on the right."

   badList = isList c     && not (isCoercion f || isNilCons f)
   txtList = "List categories [C] can only be formed by rules labeled _, [], (:), or (:[])."

   badSpecial  = elem c [ Cat x | x <- specialCatsP] && not (isCoercion f)

   badMissing  = not (null missing)
   missing     = filter (`notElem` defineds) [catToStr c | Left c <- rhs]
     where
     defineds = tokenNames cf ++ specialCatsP ++ map (catToStr . valCat) (cfgRules cf)

   badTypeName = not (null badTypes)
   badTypes = filter isBadType $ cat : [c | Left c <- rhs]
     where
     isBadType (ListCat c)    = isBadType c
     isBadType (CoercCat c _) = isBadCatName c
     isBadType (Cat s)        = isBadCatName s
     isBadType (TokenCat s)   = isBadCatName s
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
      fun <- Abs.Id <$> mkName ident rhs
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
