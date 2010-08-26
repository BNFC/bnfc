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


module GetCF where

import Directory	( doesFileExist, renameFile )
import Monad		( when )

import CF
import Utils
import ParBNF
import List(nub,partition)
import qualified AbsBNF as Abs
-- import LexBNF
import ErrM
import Char
import TypeChecker

readCF :: FilePath -> IO CF
readCF f = tryReadCF f >>= return . fst

tryReadCF :: FilePath -> IO (CF,Bool)
tryReadCF file = do
  (cfp,m) <- tryReadCFP file
  return (cfp2cf cfp, m)

tryReadCFP :: FilePath -> IO (CFP,Bool)
tryReadCFP file = do
  putStrLn $ "\nReading grammar from " ++ file
  s <- readFile file
  let (cfp,msg') = getCFP s
      cf = cfp2cf cfp
      msg = case checkDefinitions cf of
		Bad err	-> msg' ++ [err]
		Ok ()	-> msg'
      ret = cfp
  if not (null msg) then do
    putStrLn $ unlines msg
    return (ret,False)
   else do
    putStrLn $ show (length (rulesOfCF cf)) +++ "rules accepted\n"

    let c3s = [(b,e) | (b,e) <- fst (comments cf), length b > 2 || length e > 2]
    if null c3s then return () else do
      putStrLn 
        "Warning: comment delimiters longer than 2 characters ignored in Haskell:"
      mapM_ putStrLn [b +++ "-" +++ e | (b,e) <- c3s]
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


-- peteg: FIXME this is racey.
-- want to be a bit smarter about whether we actually generate the file
-- or save it... e.g. ErrM.hs need not be regenerated if it exists.
writeFileRep :: FilePath -> String -> IO ()
writeFileRep f s =
    do exists <- doesFileExist f
       backedUp <- if exists
		     then do let fbak = f ++ ".bak"
		             renameFile f fbak
			     return $ " (saving old file as " ++ fbak ++ ")"
		     else return ""
       putStrLn $ "writing file " ++ f ++ backedUp
       writeFile f s

getCF :: String -> (CF, [String])
getCF s = let (cfp,msg) = getCFP s in (cfp2cf cfp, msg)

getCFP :: String -> (CFP, [String])
getCFP s = (cf,msgs ++ msgs1) where
  (cf,msgs1) = ((exts,ruls2),msgs2)
  (ruls2,msgs2) = untag $ partition (isRule) $ map (checkRule cf00) $ rulesOfCFP cf0
  untag (ls,rs) = ([c | Left c <- ls], [c | Right c <- rs])
  isRule = either (const True) (const False)
  cf00 = cfp2cf cf0
  (cf0@(exts,_),msgs) = (revs . srt . conv . pGrammar . myLexer) s
  srt rs = let rules              = [r | Left (Right r) <- rs]
	       literals           = nub  [lit | xs <- map (snd . snd) rules,
					        (Left lit) <- xs,
					        elem lit specialCatsP]
	       pragma             = [r | Left (Left r) <- rs]
	       errors             = [s | Right s <- rs, not (null s)]
	       (symbols,keywords) = partition notIdent reservedWords
               notIdent s         = null s || not (isAlpha (head s)) || any (not . isIdentRest) s
               isIdentRest c      = isAlphaNum c || c == '_' || c == '\''
	       reservedWords      = nub [t | (_,(_,its)) <- rules, Right t <- its]
               cats               = []
	    in (((pragma,(literals,symbols,keywords,cats)),rules),errors)
  revs (cf@((pragma,(literals,symbols,keywords,_)),rules),errors) =
    (((pragma,
       (literals,symbols,keywords,findAllReversibleCats (cfp2cf cf))),rules),errors)

conv :: Err Abs.Grammar -> [Either (Either Pragma RuleP) String]
conv (Bad s)                 = [Right s]
conv (Ok (Abs.Grammar defs)) = map Left $ concatMap transDef defs

transDef :: Abs.Def -> [Either Pragma RuleP]
transDef x = case x of
 Abs.Rule label cat items -> 
   [Right (transLabel label,(transCat cat,map transItem items))]
 Abs.Comment str               -> [Left $ CommentS str]
 Abs.Comments str0 str         -> [Left $ CommentM (str0,str)]
 Abs.Token ident reg           -> [Left $ TokenReg (transIdent ident) False reg]
 Abs.PosToken ident reg        -> [Left $ TokenReg (transIdent ident) True reg]
 Abs.Entryp idents             -> [Left $ EntryPoints (map transIdent idents)]
 Abs.Internal label cat items  -> 
   [Right (transLabel label,(transCat cat,(Left "#":(map transItem items))))]
 Abs.Separator size ident str -> map  (Right . cf2cfpRule) $ separatorRules size ident str
 Abs.Terminator size ident str -> map  (Right . cf2cfpRule) $ terminatorRules size ident str
 Abs.Coercions ident int -> map  (Right . cf2cfpRule) $ coercionRules ident int
 Abs.Rules ident strs -> map (Right . cf2cfpRule) $ ebnfRules ident strs
 Abs.Layout ss      -> [Left $ Layout ss]
 Abs.LayoutStop ss  -> [Left $ LayoutStop ss]
 Abs.LayoutTop      -> [Left $ LayoutTop]
 Abs.Function f xs e -> [Left $ FunDef (transIdent f) (map transArg xs) (transExp e)]

separatorRules :: Abs.MinimumSize -> Abs.Cat -> String -> [Rule]
separatorRules size c s = if null s then terminatorRules size c s else ifEmpty [
  ("(:[])", (cs,[Left c'])),
  ("(:)",   (cs,[Left c', Right s, Left cs]))
  ]
 where 
   c' = transCat c
   cs = "[" ++ c' ++ "]"
   ifEmpty rs = if (size == Abs.MNonempty) then rs else (("[]", (cs,[])) : rs)

terminatorRules :: Abs.MinimumSize -> Abs.Cat -> String -> [Rule]
terminatorRules size c s = [
  ifEmpty,
  ("(:)",   (cs,Left c' : s' [Left cs]))
  ]
 where 
   c' = transCat c
   cs = "[" ++ c' ++ "]"
   s' its = if null s then its else (Right s : its)
   ifEmpty = if (size == Abs.MNonempty) 
                then ("(:[])",(cs,[Left c'] ++ if null s then [] else [Right s]))
                else ("[]",   (cs,[]))

coercionRules :: Abs.Ident -> Integer -> [Rule]
coercionRules (Abs.Ident c) n = 
   ("_", (c,               [Left (c ++ "1")])) :
  [("_", (c ++ show (i-1), [Left (c ++ show i)])) | i <- [2..n]] ++
  [("_", (c ++ show n,     [Right "(", Left c, Right ")"]))]

ebnfRules :: Abs.Ident -> [Abs.RHS] -> [Rule]
ebnfRules (Abs.Ident c) rhss = 
  [(mkFun k c its, (c, map transItem its)) | (k, Abs.RHS its) <- zip [1 :: Int ..] rhss]
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
 Abs.ListCat cat  -> "[" ++ (transCat cat) ++ "]"
 Abs.IdCat id     -> transIdent id

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

