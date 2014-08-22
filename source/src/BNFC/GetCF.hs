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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}


module BNFC.GetCF(parseCF, parseCFP) where

import Control.Monad		( when )

import BNFC.CF
import BNFC.Utils
import ParBNF
import Data.List(nub,partition)
import qualified AbsBNF as Abs
import Data.Maybe (catMaybes)
import Data.Either (partitionEithers)
import ErrM
import Data.Char
import BNFC.TypeChecker
import BNFC.Options
import Data.List (isPrefixOf)

parseCF :: SharedOptions -> Target -> String -> IO CF
parseCF opts t s = parseCFP opts t s >>= return . cfp2cf

parseCFP :: SharedOptions -> Target -> String -> IO CFP
parseCFP opts target content = do
  let (cfp,msgs1) = getCFP (cnf opts) content
      cf = cfp2cf cfp
      msgs2 = case checkDefinitions cf of
        Bad err -> [err]
        Ok ()   -> []
      msgs3 = checkTokens cf
      msg = msgs1++msgs2 -- ++ msgs3 -- in a future version
      ret = cfp

  let reserved = [lang opts | target == TargetJava ]

  case filter (not . isDefinedRule) $ notUniqueNames reserved cf of
    ns@(_:_)
      | not (target `elem` [TargetHaskell,TargetHaskellGadt,TargetOCaml]) -> do
        fail $ "ERROR: names not unique: " ++ unwords ns
    ns -> do
      case ns of
        _:_ -> do
          putStrLn $ "Warning: names not unique: " ++ unwords ns
          putStrLn "This can be an error in other back ends."
        _ -> return ()
      putStrLn $ unlines msgs3
      if not (null msg) then do
         fail $ unlines msg
       else do
         putStrLn $ show (length (rulesOfCF cf)) +++ "rules accepted\n"
         let c3s = [(b,e) | (b,e) <- fst (comments cf), length b > 2 || length e > 2]
         if null c3s then return () else do
           putStrLn
             "Warning: comment delimiters longer than 2 characters ignored in Haskell:"
           mapM_ putStrLn [b +++ "-" +++ e | (b,e) <- c3s]
         return cfp

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

nilCFP :: CFP
nilCFP = CFG (([],([],[],[],[])),[])

getCFP :: Bool -> String -> (CFP,[String])
getCFP cnf s = case pGrammar . myLexer $ s of
  Bad s -> (nilCFP,[s])
  Ok (Abs.Grammar defs0) -> (cf0,msgs)
    where (pragma,rules0) = partitionEithers $ concatMap transDef $ defs
          (defs,inlineDelims) = if cnf then (defs0,id) else removeDelims defs0
          rules = inlineDelims rules0
          msgs = catMaybes $ map (checkRule (cfp2cf cf0)) (rulesOfCF cf0)
          cf0 = revs srt
          srt = let    literals           = nub [lit | xs <- map rhsRule rules,
               	         		         Left lit <- xs,
               	         		         elem lit specialCatsP]
                       (symbols,keywords) = partition notIdent reservedWords
                       notIdent s         = null s || not (isAlpha (head s)) || any (not . isIdentRest) s
                       isIdentRest c      = isAlphaNum c || c == '_' || c == '\''
                       reservedWords      = nub [t | r <- rules, Right t <- rhsRule r]
                   in CFG((pragma,(literals,symbols,keywords,[])),rules)
          revs cf1@(CFG((pragma,(literals,symbols,keywords,_)),rules)) =
              CFG((pragma,(literals,symbols,keywords,findAllReversibleCats (cfp2cf cf1))),rules)

removeDelims :: [Abs.Def] -> ([Abs.Def], [RuleP] -> [RuleP])
removeDelims xs = (ys ++ map delimToSep ds,
                   foldr (.) id [map (inlineDelim' d) | d <- ds])
  where
    (ds,ys) = partition isDelim xs
    isDelim (Abs.Delimiters _ _ _ _ _) = True
    isDelim _ = False
    
    inlineDelim :: Abs.Def -> Either Cat String ->  [Either Cat String]
    inlineDelim (Abs.Delimiters cat open close separ sz) (Left c)
      | c == ListCat (transCat cat) = [Right open, Left c, Right close]
    inlineDelim _ x = [x]
    
    inlineDelim' :: Abs.Def -> RuleP -> RuleP
    inlineDelim' d@(Abs.Delimiters cat _ _ _ _) r@(Rule f c rhs) 
      | c == ListCat (transCat cat) = r
      | otherwise = Rule f c (concatMap (inlineDelim d) rhs)


    delimToSep (Abs.Delimiters cat open close (Abs.SepTerm  s) sz) = Abs.Terminator sz cat s
    delimToSep (Abs.Delimiters cat open close (Abs.SepSepar s) sz) = Abs.Separator  sz cat s
    delimToSep (Abs.Delimiters cat open close (Abs.SepNone   ) sz) = Abs.Terminator sz cat ""
    delimToSep x = x
    
transDef :: Abs.Def -> [Either Pragma RuleP]
transDef x = case x of
 Abs.Rule label cat items ->
   [Right $ Rule (transLabel label) (transCat cat) (map transItem items)]
 Abs.Comment str               -> [Left $ CommentS str]
 Abs.Comments str0 str         -> [Left $ CommentM (str0,str)]
 Abs.Token ident reg           -> [Left $ TokenReg (transIdent ident) False reg]
 Abs.PosToken ident reg        -> [Left $ TokenReg (transIdent ident) True reg]
 Abs.Entryp idents             -> [Left $ EntryPoints (map (strToCat .transIdent) idents)]
 Abs.Internal label cat items  ->
   [Right $ Rule (transLabel label) (transCat cat) (Left InternalCat:(map transItem items))]
 Abs.Separator size ident str -> map  (Right . cf2cfpRule) $ separatorRules size ident str
 Abs.Terminator size ident str -> map  (Right . cf2cfpRule) $ terminatorRules size ident str
 Abs.Delimiters a b c d e -> map  (Right . cf2cfpRule) $ delimiterRules a b c d e
 Abs.Coercions ident int -> map  (Right . cf2cfpRule) $ coercionRules ident int
 Abs.Rules ident strs -> map (Right . cf2cfpRule) $ ebnfRules ident strs
 Abs.Layout ss      -> [Left $ Layout ss]
 Abs.LayoutStop ss  -> [Left $ LayoutStop ss]
 Abs.LayoutTop      -> [Left $ LayoutTop]
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
       a' = '@':'@':(show a)
       c  = '@':'{':(show a)
       d  = '@':'}':(show a)
       -- optionally separated concat. of x and y categories.
       x // y = (Left x :
                 [Right t | Abs.SepSepar t <- [sep]] ++
                 [Left y ])
       termin = case sep of
                  Abs.SepSepar t -> [Right t]
                  Abs.SepTerm  t -> [Right t]
                  _ -> []
       optFinal = case (sep,size) of
         (Abs.SepSepar t,_) -> True
         (Abs.SepTerm _,Abs.MNonempty) -> True
         (Abs.SepNone,Abs.MNonempty) -> True
         _ -> False
       optEmpty = case sep of
         Abs.SepSepar _ -> size == Abs.MEmpty
         _ -> False


separationRules :: Abs.Cat -> Abs.Separation -> Abs.MinimumSize -> [Rule]
separationRules c Abs.SepNone size = terminatorRules size c ""
separationRules c (Abs.SepTerm t) size = terminatorRules size c t
separationRules c (Abs.SepSepar t) size = separatorRules size c t


separatorRules :: Abs.MinimumSize -> Abs.Cat -> String -> [Rule]
separatorRules size c s = if null s then terminatorRules size c s else ifEmpty [
  Rule "(:[])" cs [Left c'],
  Rule "(:)"   cs [Left c', Right s, Left cs]
  ]
 where
   c' = transCat c
   cs = ListCat c'
   ifEmpty rs = if (size == Abs.MNonempty)
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
   s' its = if null s then its else (Right s : its)
   ifEmpty = if (size == Abs.MNonempty)
                then Rule "(:[])" cs ([Left c'] ++ if null s then [] else [Right s])
                else Rule "[]" cs []

coercionRules :: Abs.Ident -> Integer -> [Rule]
coercionRules (Abs.Ident c) n =
   Rule "_" (Cat c)            [Left (CoercCat c 1)] :
  [Rule "_" (CoercCat c (i-1)) [Left (CoercCat c i)] | i <- [2..n]] ++
  [Rule "_" (CoercCat c n)     [Right "(", Left (Cat c), Right ")"]]

ebnfRules :: Abs.Ident -> [Abs.RHS] -> [Rule]
ebnfRules (Abs.Ident c) rhss =
  [Rule (mkFun k c its) (strToCat c) (map transItem its)
     | (k, Abs.RHS its) <- zip [1 :: Int ..] rhss]
 where
   mkFun k c i = case i of
     [Abs.Terminal s]  -> c' ++ "_" ++ mkName k s
     [Abs.NTerminal n] -> c' ++ identCat (transCat n)
     _ -> c' ++ "_" ++ show k
   c' = c --- normCat c
   mkName k s = if all (\c -> isAlphaNum c || elem c "_'") s
                   then s else show k

transItem :: Abs.Item -> Either Cat String
transItem x = case x of
 Abs.Terminal str   -> Right str
 Abs.NTerminal cat  -> Left (transCat cat)

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
    Abs.Var x	    -> App (transIdent x) []
    Abs.Cons e1 e2  -> cons e1 (transExp e2)
    Abs.List es	    -> foldr cons nil es
    Abs.LitInt x    -> LitInt x
    Abs.LitDouble x -> LitDouble x
    Abs.LitChar x   -> LitChar x
    Abs.LitString x -> LitString x
  where
    cons e1 e2 = App "(:)" [transExp e1, e2]
    nil	       = App "[]" []

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
checkRule cf (Rule _ (Cat ('@':_)) rhs) = Nothing -- Generated by a pragma; it's a trusted category
checkRule cf (Rule (f,_) cat rhs)
  | badCoercion    = Just $ "Bad coercion in rule" +++ s
  | badNil         = Just $ "Bad empty list rule" +++ s
  | badOne         = Just $ "Bad one-element list rule" +++ s
  | badCons        = Just $ "Bad list construction rule" +++ s
  | badList        = Just $ "Bad list formation rule" +++ s
  | badSpecial     = Just $ "Bad special category rule" +++ s
  | badTypeName    = Just $ "Bad type name" +++ unwords (map show badtypes) +++ "in" +++ s
  | badFunName     = Just $ "Bad constructor name" +++ f +++ "in" +++ s
  | badMissing     = Just $ "No production for" +++ unwords missing ++
                             ", appearing in rule" +++ s
  | otherwise      = Nothing
 where
   s  = f ++ "." +++ show cat +++ "::=" +++ unwords (map (either show show) rhs) -- Todo: consider using the show instance of Rule
   c  = normCat cat
   cs = [normCat c | Left c <- rhs]
   badCoercion = isCoercion f && not ([c] == cs) 
   badNil      = isNilFun f   && not (isList c && null cs)
   badOne      = isOneFun f   && not (isList c && cs == [catOfList c])
   badCons     = isConsFun f  && not (isList c && cs == [catOfList c, c])
   badList     = isList c     &&
                 not (isCoercion f || isNilCons f)
   badSpecial  = elem c specialCatsP && not (isCoercion f)

   badMissing  = not (null missing)
   missing     = filter nodef [show c | Left c <- rhs]
   nodef t = notElem t defineds
   defineds =
    show InternalCat : tokenNames cf ++ (map show specialCatsP) ++ map (show . valCat) (rulesOfCF cf)
   badTypeName = not (null badtypes)
   badtypes = filter isBadType $ cat : [c | Left c <- rhs]
   isBadType (ListCat c) = isBadType c
   isBadType InternalCat = False
   isBadType (CoercCat c _) = isBadCatName c
   isBadType (Cat s) = isBadCatName s
   isBadCatName s = not (isUpper (head s) || s == show InternalCat || (head s == '@'))
   badFunName = not (all (\c -> isAlphaNum c || c == '_') f {-isUpper (head f)-}
                       || isCoercion f || isNilCons f)
