{-
    BNF Converter: Alex 3.0 Generator
    Copyright (C) 2012  Author:  Antti-Juhani Kaijanaho
    Copyright (C) 2004  Author:  Peter Gammie
    (C)opyright 2003, {aarne,markus,peteg} at cs dot chalmers dot se

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

module BNFC.Backend.Haskell.CFtoAlex3Incremental (cf2alex3inc) where

import BNFC.CF
import Data.List

-- For BNFC.Backend.Haskell.RegToAlex, see below.
import AbsBNF
import Data.Char

cf2alex3inc :: String -> String -> String -> Bool -> Bool -> CF -> String
cf2alex3inc name errMod shareMod shareStrings byteStrings cf =
  unlines $ concat $ intersperse [""] [
    prelude name errMod shareMod shareStrings byteStrings,
    cMacros,
    rMacros cf,
    restOfAlex shareMod shareStrings byteStrings cf
   ]

prelude :: String -> String -> String -> Bool -> Bool -> [String]
prelude name _errMod _shareMod _shareStrings _byteStrings = [
  "-- -*- haskell -*-",
  "-- This Alex file was machine-generated by the BNF converter",
  "{",
  "{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}",
  "{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}",
  "{-# OPTIONS -fno-warn-incomplete-patterns #-}",
  "{-# OPTIONS_GHC -w #-}",
  "module " ++ name ++ " where",
  "",
  "import qualified Data.Bits",
  "import Data.Word (Word8)",
  "",
  "import Prelude hiding (null, foldl, foldr)",
  "import Data.Monoid",
  "import Data.FingerTree",
  "import qualified Data.Sequence as S",
  "import Data.Foldable (toList, foldl, foldr)",
  "}",
  ""
  ]

cMacros :: [String]
cMacros = [
  "$l = [a-zA-Z\\192 - \\255] # [\\215 \\247]    -- isolatin1 letter FIXME",
  "$c = [A-Z\\192-\\221] # [\\215]    -- capital isolatin1 letter FIXME",
  "$s = [a-z\\222-\\255] # [\\247]    -- small isolatin1 letter FIXME",
  "$d = [0-9]                -- digit",
  "$i = [$l $d _ ']          -- identifier character",
  "$u = [\\0-\\255]          -- universal: any character"
  ]

rMacros :: CF -> [String]
rMacros cf =
  let symbs = symbols cf
  in
  (if null symbs then [] else [
   "@rsyms =    -- symbols and non-identifier-like reserved words",
   "   " ++ unwords (intersperse "|" (map mkEsc symbs))
   ])
 where
  mkEsc = unwords . esc
  esc s = if null a then rest else show a : rest
      where (a,r) = span isAlphaNum s
            rest = case r of
                       [] -> []
                       (c:xs) -> s : esc xs
                         where s = if isPrint c then ['\\',c]
                                                else '\\':show (ord c)

restOfAlex :: String -> Bool -> Bool -> CF -> [String]
restOfAlex shareMod shareStrings byteStrings cf = [
  ":-",
  lexComments (comments cf),
  "$white+ ;",
  pTSpec (symbols cf),

  userDefTokenTypes,
  ident,

  ifC "String" ("\\\" ([$u # [\\\" \\\\ \\n]] | (\\\\ (\\\" | \\\\ | \\' | n | t)))* \\\"" ++
                  "{ tok (\\p s -> PT p (TL $ share $ unescapeInitTail s)) }"),
  ifC "Char"    "\\\' ($u # [\\\' \\\\] | \\\\ [\\\\ \\\' n t]) \\'  { tok (\\p s -> PT p (TC $ share s))  }",
  ifC "Integer" "$d+      { tok (\\p s -> PT p (TI $ share s))    }",
  ifC "Double"  "$d+ \\. $d+ (e (\\-)? $d+)? { tok (\\p s -> PT p (TD $ share s)) }",
  "",
  "{",
  "",
  "tok f p s = f p s",
  "",
  "share :: "++stringType++" -> "++stringType,
  "share = " ++ if shareStrings then "shareString" else "id",
  "",
  "data Tok =",
  "   TS !("++stringType++") !Int    -- reserved words and symbols",
  " | TL !("++stringType++")         -- string literals",
  " | TI !("++stringType++")         -- integer literals",
  " | TV !("++stringType++")         -- identifiers",
  " | TD !("++stringType++")         -- double precision float literals",
  " | TC !("++stringType++")         -- character literals",
  userDefTokenConstrs,
  " deriving (Eq,Show,Ord)",
  "",
  "data Token = ",
  "   PT  Posn Tok",
  " | Err Posn",
  "  deriving (Eq,Show,Ord)",
  "",
  "tokenPos (PT (Pn _ l _) _ :_) = \"line \" ++ show l",
  "tokenPos (Err (Pn _ l _) :_) = \"line \" ++ show l",
  "tokenPos _ = \"end of file\"",
  "",
  "tokenPosn (PT p _) = p",
  "tokenPosn (Err p) = p",
  "tokenLineCol = posLineCol . tokenPosn",
  "posLineCol (Pn _ l c) = (l,c)",
  "mkPosToken t@(PT p _) = (posLineCol p, prToken t)",
  "",
  "prToken t = case t of",
  "  PT _ (TS s _) -> s",
  "  PT _ (TL s)   -> s",
  "  PT _ (TI s)   -> s",
  "  PT _ (TV s)   -> s",
  "  PT _ (TD s)   -> s",
  "  PT _ (TC s)   -> s",
  userDefTokenPrint,
  "",
  "data BTree = N | B ("++stringType++") Tok BTree BTree deriving (Show)",
  "",
  "eitherResIdent :: ("++stringType++" -> Tok) -> "++stringType++" -> Tok",
  "eitherResIdent tv s = treeFind resWords",
  "  where",
  "  treeFind N = tv s",
  "  treeFind (B a t left right) | s < a  = treeFind left",
  "                              | s > a  = treeFind right",
  "                              | s == a = t",
  "",
  "resWords = " ++ (show $ sorted2tree $ cfTokens $ cf),
  "   where b s n = let bs = "++stringPack++" s",
  "                  in B bs (TS bs n)",
  "",
  "unescapeInitTail :: "++stringType++" -> "++stringType++"",
  "unescapeInitTail = unesc . tail . toList where",
  "  unesc s = case s of",
  "    '\\\\':c:cs | elem c ['\\\"', '\\\\', '\\\''] -> S.singleton c <> unesc cs",
  "    '\\\\':'n':cs  -> S.singleton '\\n' <> unesc cs",
  "    '\\\\':'t':cs  -> S.singleton '\\t' <> unesc cs",
  "    '\"':[]    -> mempty",
  "    c:cs      -> S.singleton c <> unesc cs",
  "    _         -> mempty",
  "",
  "-------------------------------------------------------------------",
  "-- Alex wrapper code.",
  "-- A modified \"posn\" wrapper.",
  "-------------------------------------------------------------------",
  "",
  "data Posn = Pn !Int !Int !Int",
  "      deriving (Eq, Show,Ord)",
  "",
  "alexStartPos :: Posn",
  "alexStartPos = Pn 0 1 1",
  "",
  "alexMove :: Posn -> Char -> Posn",
  "alexMove (Pn a l c) '\\t' = Pn (a+1)  l     (((c+7) `div` 8)*8+1)",
  "alexMove (Pn a l c) '\\n' = Pn (a+1) (l+1)   1",
  "alexMove (Pn a l c) _    = Pn (a+1)  l     (c+1)",
  "",
  "  -- | Encode a Haskell String to a list of Word8 values, in UTF8 format.",
  "utf8Encode :: Char -> [Word8]",
  "utf8Encode = map fromIntegral . go . ord",
  " where",
  "  go oc",
  "   | oc <= 0x7f       = [oc]",
  "",
  "   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)",
  "                        , 0x80 + oc Data.Bits..&. 0x3f",
  "                        ]",
  "",
  "   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)",
  "                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)",
  "                        , 0x80 + oc Data.Bits..&. 0x3f",
  "                        ]",
  "   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)",
  "                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)",
  "                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)",
  "                        , 0x80 + oc Data.Bits..&. 0x3f",
  "                        ]",
  "",
  "", -- Here comes the LexGen code
  "",
  "-- Generic template",
  "type State = Int",
  "type Transition v = State -> (Tokens v) -- Transition from in state to Tokens",
  "-- Wrapper template?",
  "data Tokens v = NoTokens",
  "            | InvalidTokens !(S.Seq Char)",
  "            | Tokens { currentSeq  :: !(FingerTree v IntToken)",
  "                      , lastToken  :: !(Suffix v)",
  "                      , outState   :: !State}",
  "-- The suffix is the the sequence of as long as possible accepting tokens.",
  "-- It can itself contain a suffix for the last token.",
  "--This is either a Sequence of tokens or one token if the it hits an accepting state with later characters",
  "-- Generic template",
  "data Suffix v = Str !(S.Seq Char)",
  "              | One !IntToken",
  "              | Multi !(Tokens v)",
  "type Size     = Sum Int",
  "--Wrapper",
  "type LexTree v = FingerTree (Table State (Tokens v),Size) Char",
  "data IntToken  = Token { lexeme   :: !(S.Seq Char)",
  "                       , token_id :: Accepts}",
  "--Wrapper template",
  "type Accepts   = AlexAcc (Posn -> S.Seq Char -> Token) ()",
  "type Table a b = Array State b",
  "",
  "tabulate :: (State,State) -> (State -> b) -> Table State b",
  "tabulate range f = listArray range [f i | i <- [fst range..snd range]]",
  "",
  "access :: Table State b -> (State -> b)",
  "access a x = a ! x",
  "-- Generic template",
  "makeTree :: Measured v IntToken => String -> LexTree v",
  "makeTree str = fromList str",
  "",
  "-- Generic template",
  "instance (Measured v IntToken) => Monoid (Table State (Tokens v)) where",
  "  mempty = tabulate stateRange (\\_ -> emptyTokens)",
  "  f `mappend` g = tabulate stateRange $ combineTokens (access f) (access g)",
  "",
  "-- Wrapper template",
  "-- The base case for when one character is lexed.",
  "instance (Measured v IntToken) => Measured (Table State (Tokens v),Size) Char where",
  "  measure c =",
  "    let bytes = utf8Encode c",
  "        sing = singleton c",
  "        cSeq = S.singleton c",
  "        baseCase in_state | in_state == -1 = InvalidTokens cSeq",
  "                          | otherwise = case foldl automata in_state bytes of",
  "          -1 -> InvalidTokens cSeq",
  "          os -> case alex_accept ! os of",
  "            AlexAccNone -> Tokens empty (Str cSeq) os",
  "            acc -> Tokens empty (One (createToken cSeq acc)) os",
  "    in (tabulate stateRange $ baseCase, Sum 1)",
  "",
  "createToken :: S.Seq Char -> Accepts -> IntToken",
  "createToken lex acc = Token lex acc",
  "",
  "createTokens :: Measured v IntToken => FingerTree v IntToken -> Suffix v -> State -> Tokens v",
  "createTokens seq suf state = if null seq",
  "                             then NoTokens",
  "                             else Tokens seq suf state",
  "-- Wrapper template",
  "invalidTokens :: S.Seq Char -> Tokens v",
  "invalidTokens s = InvalidTokens s",
  "",
  "-- Wrapper template",
  "emptyTokens :: Tokens v",
  "emptyTokens = NoTokens",
  "",
  "--------- Combination functions, the conquer step",
  "",
  "-- Generic template",
  "-- Combines two transition maps",
  "combineTokens :: Measured v IntToken => Transition v -> Transition v -> Transition v",
  "combineTokens trans1 trans2 in_state | isInvalid toks1 = toks1",
  "                                     | isEmpty toks1   = trans2 in_state",
  "                                     | otherwise = combineWithRHS toks1 trans2",
  "  where toks1 = trans1 in_state",

  "-- Generic template",
  "-- Tries to merge tokens first, if it can't it either appends the token or calls",
  "-- itself if the suffix contains Tokens instaed of a single token.",
  "combineWithRHS :: Measured v IntToken => Tokens v -> Transition v -> Tokens v",
  "combineWithRHS toks1 trans2 | isEmpty toks2 = toks1",
  "                            | isValid toks2 =",
  "    let toks2' = mergeTokens (lastToken toks1) toks2 trans2",
  "    in appendTokens seq1 toks2'",
  "                            | otherwise     = case lastToken toks1 of",
  "    Multi suffToks ->",
  "      let toks2' = combineWithRHS suffToks trans2 -- try to merge suffix",
  "      in appendTokens seq1 toks2'",
  "    One tok -> appendTokens (seq1 |> tok) (trans2 startState)",
  "    Str s -> invalidTokens s",
  "  where toks2 = trans2 $ outState toks1",
  "        seq1 = currentSeq toks1",
  "-- Generic template",
  "-- Creates one token from the last token of the first sequence and and the first",
  "-- token of the second sequence and inserts it between the init of the first",
  "-- sequence and the tail of the second sequence",
  "mergeTokens :: Measured v IntToken => Suffix v -> Tokens v -> Transition v -> Tokens v",
  "mergeTokens suff1 toks2 trans2 = case viewl (currentSeq toks2) of",
  "  token2 :< seq2' -> let newToken = mergeToken suff1 token2",
  "                     in toks2 {currentSeq = newToken <| seq2'}",
  "  EmptyL -> case alex_accept ! out_state of",
  "    AlexAccNone -> toks2 {lastToken = mergeSuff suff1 (lastToken toks2) trans2}",
  "    acc -> let lex = suffToStr suff1 <> suffToStr (lastToken toks2)",
  "           in toks2 {lastToken = One $ createToken lex acc}",
  "  where out_state = outState toks2",
  "",
  "-- Generic template",
  "-- Creates on token from a suffix and a token",
  "mergeToken :: Suffix v -> IntToken -> IntToken",
  "mergeToken suff1 token2 = token2 {lexeme = suffToStr suff1 <> lexeme token2}",
  "",
  "-- Generic template",
  "-- Creates the apropiet new suffix from two suffixes",
  "mergeSuff :: Measured v IntToken => Suffix v -> Suffix v -> Transition v -> Suffix v",
  "mergeSuff (Multi toks1) suff2 trans2 = Multi $ -- O(n^2)",
  "  let newToks = combineWithRHS toks1 trans2",
  "  in if isValid $ newToks",
  "     then newToks",
  "     else toks1 {lastToken = mergeSuff (lastToken toks1) suff2 trans2}",
  "mergeSuff (Str s1) suff2 _ = Str $ s1 <> suffToStr suff2",
  "mergeSuff (One token1) (Str s) trans2 =",
  "  let toks2 = trans2 startState",
  "  in if isValid toks2",
  "     then Multi $ toks2 {currentSeq = token1 <| currentSeq toks2}",
  "     else Multi $ createTokens (singleton token1) (Str s) (-1)",
  "mergeSuff suff1 (One token2) _ = One $ mergeToken suff1 token2 -- O(n)",
  "mergeSuff suff1 (Multi toks2) trans2 = Multi $ mergeTokens suff1 toks2 trans2 -- O(n^2)",
  "",
  "-- Generic template",
  "-- Prepends a sequence of tokens on the sequence in Tokens",
  "appendTokens :: Measured v IntToken => FingerTree v IntToken -> Tokens v -> Tokens v",
  "appendTokens seq1 toks2 | isValid toks2 = toks2 {currentSeq = seq1 <> currentSeq toks2}",
  "                        | otherwise = toks2",
  "-- Wrapper template",
  "isValid :: Tokens v -> Bool",
  "isValid (Tokens _ _ _) = True",
  "isValid _ = False",
  "",
  "-- Wrapper template",
  "isEmpty :: Tokens v -> Bool",
  "isEmpty NoTokens = True",
  "isEmpty _        = False",
  "",
  "-- Wrapper template",
  "isInvalid :: Tokens v -> Bool",
  "isInvalid (InvalidTokens _) = True",
  "isInvalid _ = False",
  "",
  "-- Generic template",
  "suffToStr :: Suffix v -> S.Seq Char",
  "suffToStr (Str s) = s",
  "suffToStr (One token) = lexeme token",
  "suffToStr (Multi toks) =",
  "  concatLexemes (currentSeq toks) <> suffToStr (lastToken toks)",
  "",
  "isAccepting :: Tokens v -> Bool",
  "isAccepting (Tokens _ suff _) = case suff of",
  "  Str _ -> False",
  "  One _ -> True",
  "  Multi toks -> isAccepting toks",
  "isAccepting NoTokens = True",
  "isAccepting _ = False",
  "",
  "-- Genereic template",
  "concatLexemes :: FingerTree v IntToken -> S.Seq Char",
  "concatLexemes = foldr ((<>) . lexeme) mempty",
  "",
  "insertAtIndex :: Measured v IntToken => String -> Int -> LexTree v -> LexTree v",
  "insertAtIndex str i tree = ",
  "  if i < 0",
  "  then error \"index must be >= 0\"",
  "  else l <> (makeTree str) <> r",
  "     where (l,r) = splitTreeAt i tree",
  "",
  "splitTreeAt :: Measured v IntToken => Int -> LexTree v -> (LexTree v,LexTree v)",
  "splitTreeAt i tree = split (\\(_,s) -> getSum s>i) tree",
  "",
  "size :: Measured v IntToken => LexTree v -> Int",
  "size tree = getSum . snd $ measure tree",
  "",
  "suffSize :: Suffix v -> Int",
  "suffSize (Multi toks) = toksSize toks",
  "suffSize (Str s) = S.length s",
  "suffSize (One (Token lex _)) = S.length lex",
  "",
  "toksSize :: Tokens v -> Int",
  "toksSize (Tokens seq suff _) = suffSize suff + foldl bla 0 seq",
  "  where bla _ (Token lex _) = S.length lex",
  "toksSize _ = 0",
  "",
  "  -- Starting state",
  "startState :: Int",
  "startState = 0",
  "-- A tuple that says how many states there are",
  "stateRange :: (Int,Int)",
  "stateRange = let (start,end) = bounds alex_accept",
  "             in (start-1,end)",
  "",
  "-- Generic",
  "-- Takes an in state and a byte and returns the corresponding out state using the DFA",
  "automata :: Int -> Word8 -> Int",
  "automata (-1) _ = -1",
  "automata s c = let base   = alex_base ! s",
  "                   ord_c  = fromEnum c",
  "                   offset = base + ord_c",
  "                   check  =  alex_check ! offset",
  "               in if (offset >= (0)) && (check == ord_c)",
  "                  then alex_table ! offset",
  "                  else alex_deflt ! s",
  "",
  "-- Not used, therefore undefined",
  "alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)",
  "alexGetByte (p, c, (b:bs), s) = Just (b, (p, c, bs, s))",
  "alexGetByte (p, _, [], s) = undefined",
  "",
  "alexInputPrevChar :: AlexInput -> Char",
  "alexInputPrevChar (p, c, bs, s) = c",
  "",
  "-- Not used, but needed for Alex templates",
  "type Byte = Word8",
  "type AlexInput = (Posn,     -- current position,",
  "                  Char,     -- previous char",
  "                  [Byte],   -- pending bytes on the current char",
  "                  "++stringType++")   -- current input string",
  "",
  "}"
  ]
 where
   stringType = "S.Seq Char"
   stringPack = "S.fromList"

   ifC cat s = if isUsedCat cf cat then s else ""
   lexComments ([],[])           = []
   lexComments (xs,s1:ys) = '\"' : s1 ++ "\"" ++ " [.]* ; -- Toss single line comments\n" ++ lexComments (xs, ys)
   lexComments (([l1,l2],[r1,r2]):xs,[]) = concat $
					[
					('\"':l1:l2:"\" ([$u # \\"), -- FIXME quotes or escape?
					(l2:"] | \\"),
					(r1:" [$u # \\"),
					(r2:"])* (\""),
					(r1:"\")+ \""),
					(r2:"\" ; \n"),
					lexComments (xs, [])
					]
   lexComments ((_:xs),[]) = lexComments (xs,[])
---   lexComments (xs,(_:ys)) = lexComments (xs,ys)

   -- tokens consisting of special symbols
   pTSpec [] = ""
   pTSpec _ = "@rsyms { tok (\\p s -> PT p (eitherResIdent (TV . share) s)) }"

   userDefTokenTypes = unlines $
     [printRegAlex exp ++
      " { tok (\\p s -> PT p (eitherResIdent (T_"  ++ name ++ " . share) s)) }"
      | (name,exp) <- tokenPragmas cf]
   userDefTokenConstrs = unlines $
     [" | T_" ++ name ++ " !"++stringType | (name,_) <- tokenPragmas cf]
   userDefTokenPrint = unlines $
     ["  PT _ (T_" ++ name ++ " s) -> s" | (name,_) <- tokenPragmas cf]

   ident =
     "$l $i*   { tok (\\p s -> PT p (eitherResIdent (TV . share) s)) }"
     --ifC "Ident"  "<ident>   ::= ^l ^i*   { ident  p = PT p . eitherResIdent TV }"


data BTree = N | B String Int BTree BTree

instance Show BTree where
    showsPrec _ N = showString "N"
    showsPrec n (B s k l r) = wrap (showString "b " . shows s  . showChar ' '. shows k  . showChar ' '
				    . showsPrec 1 l . showChar ' '
				    . showsPrec 1 r)
	where wrap f = if n > 0 then showChar '(' . f . showChar ')' else f

sorted2tree :: [(String,Int)] -> BTree
sorted2tree [] = N
sorted2tree xs = B x n (sorted2tree t1) (sorted2tree t2) where
  (t1,((x,n):t2)) = splitAt (length xs `div` 2) xs


-------------------------------------------------------------------
-- Inlined version of @BNFC.Backend.Haskell.RegToAlex@.
-- Syntax has changed...
-------------------------------------------------------------------

-- modified from pretty-printer generated by the BNF converter

-- the top-level printing method
printRegAlex :: Reg -> String
printRegAlex = render . prt 0

-- you may want to change render and parenth

render :: [String] -> String
render = rend 0
    where rend :: Int -> [String] -> String
	  rend i ss = case ss of
		        "["      :ts -> cons "["  $ rend i ts
			"("      :ts -> cons "("  $ rend i ts
			t  : "," :ts -> cons t    $ space "," $ rend i ts
		        t  : ")" :ts -> cons t    $ cons ")"  $ rend i ts
			t  : "]" :ts -> cons t    $ cons "]"  $ rend i ts
			t        :ts -> space t   $ rend i ts
			_            -> ""

	  cons s t  = s ++ t
	  new i s   = s
	  space t s = if null s then t else t ++ " " ++ s

parenth :: [String] -> [String]
parenth ss = ["("] ++ ss ++ [")"]

-- the printer class does the job
class Print a where
  prt :: Int -> a -> [String]
  prtList :: [a] -> [String]
  prtList = concat . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ c = if isAlphaNum c then [[c]] else ['\\':[c]]
  prtList s = map (concat . prt 0) s

prPrec :: Int -> Int -> [String] -> [String]
prPrec i j = if j<i then parenth else id

instance Print Ident where
  prt _ (Ident i) = [i]

instance Print Reg where
  prt i e = case e of
   RSeq reg0 reg -> prPrec i 2 (concat [prt 2 reg0 , prt 3 reg])
   RAlt reg0 reg -> prPrec i 1 (concat [prt 1 reg0 , ["|"] , prt 2 reg])
   RMinus reg0 reg -> prPrec i 1 (concat [prt 2 reg0 , ["#"] , prt 2 reg])
   RStar reg -> prPrec i 3 (concat [prt 3 reg , ["*"]])
   RPlus reg -> prPrec i 3 (concat [prt 3 reg , ["+"]])
   ROpt reg  -> prPrec i 3 (concat [prt 3 reg , ["?"]])
   REps  -> prPrec i 3 (["$"])
   RChar c -> prPrec i 3 (concat [prt 0 c])
   RAlts str -> prPrec i 3 (concat [["["],prt 0 str,["]"]])
   RSeqs str -> prPrec i 2 (concat (map (prt 0) str))
   RDigit  -> prPrec i 3 (concat [["$d"]])
   RLetter  -> prPrec i 3 (concat [["$l"]])
   RUpper  -> prPrec i 3 (concat [["$c"]])
   RLower  -> prPrec i 3 (concat [["$s"]])
   RAny  -> prPrec i 3 (concat [["$u"]])
