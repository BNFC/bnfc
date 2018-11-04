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


module BNFC.GetCF where

import qualified AbsBNF as Abs
import ParBNF
import BNFC.CF
import BNFC.Options
import BNFC.TypeChecker
import BNFC.Utils

import Control.Arrow (left)
import Control.Monad.State
import Data.Char
import Data.Either (partitionEithers)
import Data.List(nub,partition)
import Data.Maybe (mapMaybe)
import ErrM
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- $setup
-- >>> import PrintBNF

parseCF :: SharedOptions -> Target -> String -> IO CF
parseCF opts t s = liftM cfp2cf (parseCFP opts t s)

parseCFP :: SharedOptions -> Target -> String -> IO CFP
parseCFP opts target content = do
  cfp <- runErr $ pGrammar (myLexer content)
                    >>= expandRules
                    >>= getCFP (cnf opts)
                    >>= markTokenCategories
  let cf = cfp2cf cfp
  runErr $ checkDefinitions cf
  let msgs3 = checkTokens cf

  let reserved = [lang opts | target == TargetJava ]

  -- Warn of fail if the grammar use non unique names
  case filter (not . isDefinedRule) $ notUniqueNames reserved cf of
    [] -> return ()
    ns| target `notElem` [TargetHaskell,TargetHaskellGadt,TargetOCaml]
      -> fail $ "ERROR: names not unique: " ++ unwords ns
      | otherwise
      -> do putStrLn $ "Warning: names not unique: " ++ unwords ns
            putStrLn "This can be an error in other back ends."

  -- Print msgs3
  putStrLn $ unlines msgs3

  -- Print the number of rules
  putStrLn $ show (length (cfgRules cf)) +++ "rules accepted\n"

  -- Print a warning if comment delimiter are bigger than 2 characters
  let c3s = [(b,e) | (b,e) <- fst (comments cf), length b > 2 || length e > 2]
  unless (null c3s) $do
      putStrLn "Warning: comment delimiters longer than 2 characters ignored in Haskell:"
      mapM_ putStrLn [b +++ "-" +++ e | (b,e) <- c3s]
  return cfp

  where
    runErr (Ok a) = return a
    runErr (Bad msg) = hPutStrLn stderr msg >> exitFailure

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
    let rules = inlineDelims rules0
        cf0 = revs srt
        srt = let literals           = nub [lit | xs <- map rhsRule rules,
                                                     Left (Cat lit) <- xs,
                                                     lit `elem` specialCatsP]
                  (symbols,keywords) = partition notIdent reservedWords
                  notIdent s         = null s || not (isAlpha (head s)) || any (not . isIdentRest) s
                  isIdentRest c      = isAlphaNum c || c == '_' || c == '\''
                  reservedWords      = nub [t | r <- rules, Right t <- rhsRule r]
              in CFG pragma literals symbols keywords [] rules
    case mapMaybe (checkRule (cfp2cf cf0)) (cfgRules cf0) of
      [] -> return ()
      msgs -> fail (unlines msgs)
    return cf0
  where
    (pragma,rules0) = partitionEithers $ concatMap transDef defs
    (defs,inlineDelims) = if cnf then (defs0,id) else removeDelims defs0
    revs cf1@CFG{..} =
        cf1 { cfgReversibleCats = findAllReversibleCats (cfp2cf cf1) }

-- | This function goes through each rule of a grammar and replace Cat "X" with
-- TokenCat "X" when "X" is a token type.
markTokenCategories :: CFP -> Err CFP
markTokenCategories cf@CFG{..} = return $ cf { cfgRules = newRules }
  where
    newRules = [ Rule f (mark c) (map (left mark) rhs) | Rule f c rhs <- cfgRules ]
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
    inlineDelim' d@(Abs.Delimiters cat _ _ _ _) r@(Rule f c rhs)
      | c == ListCat (transCat cat) = r
      | otherwise = Rule f c (concatMap (inlineDelim d) rhs)
    inlineDelim' _ _ = error "Not a delimiters pragma"


    delimToSep (Abs.Delimiters cat _ _ (Abs.SepTerm  s) sz) = Abs.Terminator sz cat s
    delimToSep (Abs.Delimiters cat _ _ (Abs.SepSepar s) sz) = Abs.Separator  sz cat s
    delimToSep (Abs.Delimiters cat _ _  Abs.SepNone     sz) = Abs.Terminator sz cat ""
    delimToSep x = x

transDef :: Abs.Def -> [Either Pragma RuleP]
transDef x = case x of
 Abs.Rule label cat items ->
   [Right $ Rule (transLabel label) (transCat cat) (concatMap transItem items)]
 Abs.Comment str               -> [Left $ CommentS str]
 Abs.Comments str0 str         -> [Left $ CommentM (str0,str)]
 Abs.Token ident reg           -> [Left $ TokenReg (transIdent ident) False reg]
 Abs.PosToken ident reg        -> [Left $ TokenReg (transIdent ident) True reg]
 Abs.Entryp idents             -> [Left $ EntryPoints (map (strToCat .transIdent) idents)]
 Abs.Internal label cat items  ->
   [Right $ Rule (transLabel label) (transCat cat) (Left InternalCat:concatMap transItem items)]
 Abs.Separator size ident str -> map  (Right . cf2cfpRule) $ separatorRules size ident str
 Abs.Terminator size ident str -> map  (Right . cf2cfpRule) $ terminatorRules size ident str
 Abs.Delimiters a b c d e -> map  (Right . cf2cfpRule) $ delimiterRules a b c d e
 Abs.Coercions ident int -> map  (Right . cf2cfpRule) $ coercionRules ident int
 Abs.Rules ident strs -> map (Right . cf2cfpRule) $ ebnfRules ident strs
 Abs.Layout ss      -> [Left $ Layout ss]
 Abs.LayoutStop ss  -> [Left $ LayoutStop ss]
 Abs.LayoutTop      -> [Left LayoutTop]
 Abs.Function f xs e -> [Left $ FunDef (transIdent f) (map transArg xs) (transExp e)]

delimiterRules :: Abs.Cat -> String -> String -> Abs.Separation -> Abs.MinimumSize -> [Rule]
delimiterRules a0 l r (Abs.SepTerm  "") size = delimiterRules a0 l r Abs.SepNone size
delimiterRules a0 l r (Abs.SepSepar "") size = delimiterRules a0 l r Abs.SepNone size
delimiterRules a0 l r sep size = [
   -- recognizing a single element
  Rule "(:[])"  (strToCat a')  (Left a : termin), -- optional terminator/separator

  -- glueing two sublists
  Rule "(++)"   (strToCat a')  [Left (strToCat a'), Left (strToCat a')],

   -- starting on either side with a delimiter
  Rule "[]"     (strToCat c)   [Right l],
  Rule (if optFinal then "(:[])" else
                         "[]")
                (strToCat d)   ([Left a | optFinal] ++ [Right r]),

   -- gathering chains
  Rule "(++)"   (strToCat c)   [Left (strToCat c), Left (strToCat a')],
  Rule "(++)"   (strToCat d)   [Left (strToCat a'), Left (strToCat d)],

   -- finally, put together left and right chains
  Rule "(++)"   as  [Left (strToCat c),Left (strToCat d)]] ++ [
  -- special rule for the empty list if necessary
  Rule "[]"     as  [Right l,Right r] | optEmpty]
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


separatorRules :: Abs.MinimumSize -> Abs.Cat -> String -> [Rule]
separatorRules size c s = if null s then terminatorRules size c s else ifEmpty [
  Rule "(:[])" cs [Left c'],
  Rule "(:)"   cs [Left c', Right s, Left cs]
  ]
 where
   c' = transCat c
   cs = ListCat c'
   ifEmpty rs = if size == Abs.MNonempty
                then rs
                else Rule "[]" cs [] : rs

terminatorRules :: Abs.MinimumSize -> Abs.Cat -> String -> [Rule]
terminatorRules size c s = [
  ifEmpty,
  Rule "(:)" cs (Left c' : s' [Left cs])
  ]
 where
   c' = transCat c
   cs = ListCat c'
   s' its = if null s then its else Right s : its
   ifEmpty = if size == Abs.MNonempty
                then Rule "(:[])" cs (Left c' : if null s then [] else [Right s])
                else Rule "[]" cs []

coercionRules :: Abs.Ident -> Integer -> [Rule]
coercionRules (Abs.Ident c) n =
   Rule "_" (Cat c)            [Left (CoercCat c 1)] :
  [Rule "_" (CoercCat c (i-1)) [Left (CoercCat c i)] | i <- [2..n]] ++
  [Rule "_" (CoercCat c n)     [Right "(", Left (Cat c), Right ")"]]

ebnfRules :: Abs.Ident -> [Abs.RHS] -> [Rule]
ebnfRules (Abs.Ident c) rhss =
  [Rule (mkFun k its) (strToCat c) (concatMap transItem its)
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
 Abs.IdCat (Abs.Ident c)     -> strToCat c

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

transIdent :: Abs.Ident -> String
transIdent x = case x of
 Abs.Ident str  -> str

transArg :: Abs.Arg -> String
transArg (Abs.Arg x) = transIdent x

transExp :: Abs.Exp -> Exp
transExp e = case e of
    Abs.App x es    -> App (transIdent x) (map transExp es)
    Abs.Var x       -> App (transIdent x) []
    Abs.Cons e1 e2  -> cons e1 (transExp e2)
    Abs.List es     -> foldr cons nil es
    Abs.LitInt x    -> LitInt x
    Abs.LitDouble x -> LitDouble x
    Abs.LitChar x   -> LitChar x
    Abs.LitString x -> LitString x
  where
    cons e1 e2 = App "(:)" [transExp e1, e2]
    nil        = App "[]" []

--------------------------------------------------------------------------------

--checkTokens :: CFG f -> [String]
checkTokens cf =
    if null ns
    then []
    else ["Warning : ", -- change to error in a future version
          "  The following tokens accept the empty string: ",
          "    "++unwords ns,
          "  This is error-prone and will not be supported in the future."]
  where
    ns = map (show.fst) . filter (nullable.snd) $ tokenPragmas cf

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
checkRule _ (Rule _ (Cat ('@':_)) _) = Nothing -- Generated by a pragma; it's a trusted category
checkRule cf (Rule (f,_) cat rhs)
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
   missing     = filter nodef [show c | Left c <- rhs]
   nodef t = t `notElem` defineds
   defineds =
    show InternalCat : tokenNames cf ++ specialCatsP ++ map (show . valCat) (cfgRules cf)
   badTypeName = not (null badtypes)
   badtypes = filter isBadType $ cat : [c | Left c <- rhs]
   isBadType (ListCat c) = isBadType c
   isBadType InternalCat = False
   isBadType (CoercCat c _) = isBadCatName c
   isBadType (Cat s) = isBadCatName s
   isBadType (TokenCat s) = isBadCatName s
   isBadCatName s = not (isUpper (head s) || s == show InternalCat || (head s == '@'))
   badFunName = not (all (\c -> isAlphaNum c || c == '_') f {-isUpper (head f)-}
                       || isCoercion f || isNilCons f)


-- | Pre-processor that converts the `rules` macros to regular rules
-- by creating unique function names for them.
-- >>> :{
-- let rules1 = Abs.Rules (Abs.Ident "Foo")
--         [ Abs.RHS [Abs.Terminal "abc"]
--         , Abs.RHS [Abs.NTerminal (Abs.IdCat (Abs.Ident "A"))]
--         , Abs.RHS [Abs.Terminal "foo", Abs.Terminal "bar"]
--         , Abs.RHS [Abs.Terminal "++"]
--         ]
-- in
-- let Ok tree = expandRules (Abs.Grammar [rules1])
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
-- let rules1 = Abs.Rules (Abs.Ident "Foo")
--         [ Abs.RHS [Abs.Terminal "foo", Abs.Terminal "bar"] ]
-- in
-- let rules2 = Abs.Rules (Abs.Ident "Foo")
--         [ Abs.RHS [Abs.Terminal "foo", Abs.Terminal "foo"] ]
-- in
-- let Ok tree = expandRules (Abs.Grammar [rules1, rules2])
-- in putStrLn (printTree tree)
-- :}
-- Foo1 . Foo ::= "foo" "bar";
-- Foo2 . Foo ::= "foo" "foo"
--
-- This is using a State monad to remember the last used index for a category.
expandRules :: Abs.Grammar -> Err Abs.Grammar
expandRules (Abs.Grammar defs) =
    return $ Abs.Grammar (concat (evalState (mapM expand defs) []))
  where
    expand (Abs.Rules ident rhss) = mapM (mkRule ident) rhss
    expand other = return [other]
    mkRule :: Abs.Ident -> Abs.RHS -> State [(String, Int)] Abs.Def
    mkRule ident (Abs.RHS rhs) = do
      fun <- liftM (Abs.LabNoP . Abs.Id . Abs.Ident) (mkName ident rhs)
      return (Abs.Rule fun (Abs.IdCat ident) rhs)
    mkName :: Abs.Ident -> [Abs.Item] -> State [(String, Int)] String
    mkName (Abs.Ident cat) [Abs.Terminal s]
      | all (\c -> isAlphaNum c || elem c ("_'" :: String)) s = return (cat ++ "_" ++ s)
    mkName (Abs.Ident cat) [Abs.NTerminal (Abs.IdCat (Abs.Ident s))] =
        return (cat ++ s)
    mkName (Abs.Ident cat) _ = do
        i <- liftM (maybe 1 (+1) . lookup cat) get
        modify ((cat, i):)
        return (cat ++ show i)
