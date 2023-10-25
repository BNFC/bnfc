{-
    BNF Converter: Alex 3.x Generator
    Copyright (C) 2012  Author:  Antti-Juhani Kaijanaho
    Copyright (C) 2004  Author:  Peter Gammie
    (C)opyright 2003, {aarne,markus,peteg} at cs dot chalmers dot se

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module BNFC.Backend.Haskell.CFtoAlex3 (cf2alex3) where

import Data.Char
import qualified Data.List as List

import BNFC.Abs
import BNFC.CF
import BNFC.Lexing         ( mkRegMultilineComment )
import BNFC.Options        ( TokenText(..) )
import BNFC.PrettyPrint
import BNFC.Utils          ( table, when, unless )

import BNFC.Backend.Common ( unicodeAndSymbols )
import BNFC.Backend.Haskell.Utils

cf2alex3 :: String -> TokenText -> CF -> String
cf2alex3 name tokenText cf =
  unlines $ List.intercalate [""] $   -- equivalent to vsep: intersperse empty lines
  [ prelude name tokenText
  , cMacros
  , rMacros cf
  , restOfAlex tokenText cf
  ]

prelude :: String -> TokenText -> [String]
prelude name tokenText = concat
  [ [ "-- Lexer definition for use with Alex 3"
    , "{"
    , "{-# OPTIONS -fno-warn-incomplete-patterns #-}"
    , "{-# OPTIONS_GHC -w #-}"
    , ""
    , "{-# LANGUAGE PatternSynonyms #-}"
    , ""
    , "module " ++ name ++ " where"
    , ""
    , "import Prelude"
    , ""
    ]
  , tokenTextImport tokenText
  , [ "import qualified Data.Bits"
    , "import Data.Char     (ord)"
    , "import Data.Function (on)"
    , "import Data.Word     (Word8)"
    , "}"
    ]
  ]

-- | Character class definitions.

cMacros :: [String]
cMacros =
  [ "-- Predefined character classes"
  , ""
  , "$c = [A-Z\\192-\\221] # [\\215]  -- capital isolatin1 letter (215 = \\times) FIXME"
  , "$s = [a-z\\222-\\255] # [\\247]  -- small   isolatin1 letter (247 = \\div  ) FIXME"
  , "$l = [$c $s]         -- letter"
  , "$d = [0-9]           -- digit"
  , "$i = [$l $d _ ']     -- identifier character"
  , "$u = [. \\n]          -- universal: any character"
  ]

-- | Regular expressions and lex actions.

rMacros :: CF -> [String]
rMacros cf = unless (null symbs)
  [ "-- Symbols and non-identifier-like reserved words"
  , ""
  , "@rsyms = " ++ List.intercalate " | " (map (unwords . esc) symbs)
  ]
  where
  symbs = unicodeAndSymbols cf
  esc :: String -> [String]
  esc s = if null a then rest else show a : rest
    where
    (a, r) = span (\ c -> isAscii c && isAlphaNum c) s
    rest = case r of
      []     -> []
      c : xs -> (if isPrint c then ['\\',c] else '\\' : show (ord c)) : esc xs

restOfAlex :: TokenText -> CF -> [String]
restOfAlex tokenText cf = concat
  [ [ ":-"
    , ""
    ]
  , lexComments $ comments cf
  , [ "-- Whitespace (skipped)"
    ,  "$white+ ;"
    , ""
    ]
  , unless (null $ unicodeAndSymbols cf)
    [ "-- Symbols"
    , "@rsyms"
    , "    { tok (eitherResIdent TV) }"
    , ""
    ]
  , userDefTokenTypes
  , [ "-- Keywords and Ident"
    , "$l $i*"
    , "    { tok (eitherResIdent TV) }"
    , ""
    ]
  , ifC catString
    [ "-- String"
    , "\\\" ([$u # [\\\" \\\\ \\n]] | (\\\\ (\\\" | \\\\ | \\' | n | t | r | f)))* \\\""
    , "    { tok (TL . unescapeInitTail) }"
    , ""
    ]
  , ifC catChar
    [ "-- Char"
    , "\\\' ($u # [\\\' \\\\] | \\\\ [\\\\ \\\' n t r f]) \\'"
    , "    { tok TC }"
    , ""
    ]
  , ifC catInteger
    [ "-- Integer"
    , "$d+"
    , "    { tok TI }"
    , ""
    ]
  , ifC catDouble
    [ "-- Double"
    , "$d+ \\. $d+ (e (\\-)? $d+)?"
    , "    { tok TD }"
    , ""
    ]
  , [ "{"
    , "-- | Create a token with position."
    , "tok :: (" ++ stringType ++ " -> Tok) -> (Posn -> Int -> " ++ stringType ++ " -> Token)"
    , "tok f p l = PT p l . f"
    , ""
    , "-- | Token without position."
    , "data Tok"
    ]
  , map ("  " ++) $ table "  " $
    [ [ "= TK {-# UNPACK #-} !TokSymbol", "-- ^ Reserved word or symbol." ]
    , [ "| TL !" ++ stringType          , "-- ^ String literal."          ]
    , [ "| TI !" ++ stringType          , "-- ^ Integer literal."         ]
    , [ "| TV !" ++ stringType          , "-- ^ Identifier."              ]
    , [ "| TD !" ++ stringType          , "-- ^ Float literal."           ]
    , [ "| TC !" ++ stringType          , "-- ^ Character literal."       ]
    ]
  , [ "  | T_" ++ name ++ " !" ++ stringType | name <- tokenNames cf ]
  , [ "  deriving (Eq, Show, Ord)"
    , ""
    , "-- | Smart constructor for 'Tok' for the sake of backwards compatibility."
    , "pattern TS :: " ++ stringType ++ " -> Int -> Tok"
    , "pattern TS t i = TK (TokSymbol t i)"
    , ""
    , "-- | Keyword or symbol tokens have a unique ID."
    , "data TokSymbol = TokSymbol"
    , "  { tsText :: " ++ stringType
    , "      -- ^ Keyword or symbol text."
    , "  , tsID   :: !Int"
    , "      -- ^ Unique ID."
    , "  } deriving (Show)"
    , ""
    , "-- | Keyword/symbol equality is determined by the unique ID."
    , "instance Eq  TokSymbol where (==)    = (==)    `on` tsID"
    , ""
    , "-- | Keyword/symbol ordering is determined by the unique ID."
    , "instance Ord TokSymbol where compare = compare `on` tsID"
    , ""
    , "-- | Token with position."
    , "data Token"
    , "  = PT  Posn Int Tok"
    , "  | Err Posn"
    , "  deriving (Eq, Show, Ord)"
    , ""
    , "-- | Pretty print a position."
    , "printPosn :: Posn -> String"
    , "printPosn (Pn _ l c) = \"line \" ++ show l ++ \", column \" ++ show c"
    , ""
    , "-- | Pretty print the position of the first token in the list."
    , "tokenPos :: [Token] -> String"
    , "tokenPos (t:_) = printPosn (tokenPosn t)"
    , "tokenPos []    = \"end of file\""
    , ""
    , "-- | Get the position of a token."
    , "tokenPosn :: Token -> Posn"
    , "tokenPosn (PT posn _len _tok) = posn"
    , "tokenPosn (Err posn)  = posn"
    , ""
    , "-- | Get start line and column of a token."
    , "tokenLineCol :: Token -> (Int, Int)"
    , "tokenLineCol = posLineCol . tokenPosn"
    , ""
    , "-- | Get line and column of a position."
    , "posLineCol :: Posn -> (Int, Int)"
    , "posLineCol (Pn _ l c) = (l,c)"
    , ""
    , "-- | Convert a token into \"position token\" form."
    , "mkPosToken :: Token -> ((Int, Int), " ++ stringType ++ ")"
    , "mkPosToken t = (tokenLineCol t, tokenText t)"
    , ""
    , "-- | Convert a token to its text."
    , "tokenText :: Token -> " ++ stringType
    , "tokenText t = case t of"
    , "  PT _ _ (TS s _) -> s"
    , "  PT _ _ (TL s)   -> " ++ applyP stringPack "show s"
    , "  PT _ _ (TI s)   -> s"
    , "  PT _ _ (TV s)   -> s"
    , "  PT _ _ (TD s)   -> s"
    , "  PT _ _ (TC s)   -> s"
    , "  Err _         -> " ++ apply stringPack "\"#error\""
    ]
  , [ "  PT _ _ (T_" ++ name ++ " s) -> s" | name <- tokenNames cf ]
  , [ ""
    , "-- | Convert a token to a string."
    , "prToken :: Token -> String"
    , "prToken t = " ++ applyP stringUnpack "tokenText t"
    , ""
    , "-- | Finite map from text to token organized as binary search tree."
    , "data BTree"
    , "  = N -- ^ Nil (leaf)."
    , "  | B " ++ stringType ++ " Tok BTree BTree"
    , "      -- ^ Binary node."
    , "  deriving (Show)"
    , ""
    , "-- | Convert potential keyword into token or use fallback conversion."
    , "eitherResIdent :: (" ++ stringType ++ " -> Tok) -> " ++ stringType ++ " -> Tok"
    , "eitherResIdent tv s = treeFind resWords"
    , "  where"
    , "  treeFind N = tv s"
    , "  treeFind (B a t left right) ="
    , "    case compare s a of"
    , "      LT -> treeFind left"
    , "      GT -> treeFind right"
    , "      EQ -> t"
    , ""
    , "-- | The keywords and symbols of the language organized as binary search tree."
    , "resWords :: BTree"
    , render $ hang "resWords =" 2 $ pretty $ sorted2tree tokens
    ]
  , unless (null tokens)
    [ "  where"
    , "  b s n = B bs (TS bs n)"
    , "    where"
    , "    bs = "++ apply stringPack "s"
    ]
  , [ ""
    , "-- | Unquote string literal."
    , "unescapeInitTail :: " ++ stringType ++ " -> " ++ stringType ++ ""
    , "unescapeInitTail = " ++ stringPack ++ " . unesc . tail . " ++ stringUnpack
    , "  where"
    , "  unesc s = case s of"
    , "    '\\\\':c:cs | elem c ['\\\"', '\\\\', '\\\''] -> c : unesc cs"
    , "    '\\\\':'n':cs  -> '\\n' : unesc cs"
    , "    '\\\\':'t':cs  -> '\\t' : unesc cs"
    , "    '\\\\':'r':cs  -> '\\r' : unesc cs"
    , "    '\\\\':'f':cs  -> '\\f' : unesc cs"
    , "    '\"':[]       -> []"
    , "    c:cs         -> c : unesc cs"
    , "    _            -> []"
    , ""
    , "-------------------------------------------------------------------"
    , "-- Alex wrapper code."
    , "-- A modified \"posn\" wrapper."
    , "-------------------------------------------------------------------"
    , ""
    , "data Posn = Pn !Int !Int !Int"
    , "  deriving (Eq, Show, Ord)"
    , ""
    , "alexStartPos :: Posn"
    , "alexStartPos = Pn 0 1 1"
    , ""
    , "alexMove :: Posn -> Char -> Posn"
    , "alexMove (Pn a l c) '\\t' = Pn (a+1)  l     (((c+7) `div` 8)*8+1)"
    , "alexMove (Pn a l c) '\\n' = Pn (a+1) (l+1)   1"
    , "alexMove (Pn a l c) _    = Pn (a+1)  l     (c+1)"
    , ""
    , "type Byte = Word8"
    , ""
    , "type AlexInput = (Posn,     -- current position,"
    , "                  Char,     -- previous char"
    , "                  [Byte],   -- pending bytes on the current char"
    , "                  " ++ stringType ++ ")   -- current input string"
    , ""
    , "tokens :: " ++ stringType ++ " -> [Token]"
    , "tokens str = go (alexStartPos, '\\n', [], str)"
    , "    where"
    , "      go :: AlexInput -> [Token]"
    , "      go inp@(pos, _, _, str) ="
    , "               case alexScan inp 0 of"
    , "                AlexEOF                   -> []"
    , "                AlexError (pos, _, _, _)  -> [Err pos]"
    , "                AlexSkip  inp' len        -> go inp'"
    , "                AlexToken inp' len act    -> act pos len (" ++ stringTake ++ " len str) : (go inp')"
    , ""
    , "alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)"
    , "alexGetByte (p, c, (b:bs), s) = Just (b, (p, c, bs, s))"
    , "alexGetByte (p, _, [], s) ="
    , "  case " ++ apply stringUncons "s" ++ " of"
    , "    " ++ stringNilP ++ "  -> Nothing"
    , "    " ++ stringConsP ++ " ->"
    , "             let p'     = alexMove p c"
    , "                 (b:bs) = utf8Encode c"
    , "              in p' `seq` Just (b, (p', c, bs, s))"
    , ""
    , "alexInputPrevChar :: AlexInput -> Char"
    , "alexInputPrevChar (p, c, bs, s) = c"
    , ""
    , "-- | Encode a Haskell String to a list of Word8 values, in UTF8 format."
    , "utf8Encode :: Char -> [Word8]"
    , "utf8Encode = map fromIntegral . go . ord"
    , "  where"
    , "  go oc"
    , "   | oc <= 0x7f       = [oc]"
    , ""
    , "   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)"
    , "                        , 0x80 + oc Data.Bits..&. 0x3f"
    , "                        ]"
    , ""
    , "   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)"
    , "                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)"
    , "                        , 0x80 + oc Data.Bits..&. 0x3f"
    , "                        ]"
    , "   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)"
    , "                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)"
    , "                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)"
    , "                        , 0x80 + oc Data.Bits..&. 0x3f"
    , "                        ]"
    , "}"
    ]
  ]
  where
  (stringType, stringTake, stringUncons, stringPack, stringUnpack, stringNilP, stringConsP) =
    case tokenText of
      StringToken     -> ("String",        "take",    "",          "id",      "id",        "[]",      "(c:s)"     )
      ByteStringToken -> ("BS.ByteString", "BS.take", "BS.uncons", "BS.pack", "BS.unpack", "Nothing", "Just (c,s)")
      TextToken       -> ("Data.Text.Text", "Data.Text.take", "Data.Text.uncons", "Data.Text.pack", "Data.Text.unpack", "Nothing", "Just (c,s)")

  apply :: String -> String -> String
  apply ""   s = s
  apply "id" s = s
  apply f    s = f ++ " " ++ s

  applyP :: String -> String -> String
  applyP ""   s = s
  applyP "id" s = s
  applyP f    s = f ++ " (" ++ s ++ ")"

  ifC :: Monoid m => TokenCat -> m -> m
  ifC = when . isUsedCat cf . TokenCat

  lexComments
    :: ( [(String, String)]  -- block comment delimiters
       , [String]            -- line  comment initiators
       ) -> [String]         -- Alex declarations
  lexComments (block, line) = concat $
    [ concatMap lexLineComment line
    , concatMap (uncurry lexBlockComment) block
    ]

  lexLineComment
    :: String   -- ^ Line comment start.
    -> [String] -- ^ Alex declaration.
  lexLineComment s =
    [ unwords [ "-- Line comment", show s ]
    , concat [ "\"", s, "\" [.]* ;" ]
    , ""
    ]

  lexBlockComment
    :: String   -- ^ Start of block comment.
    -> String   -- ^ End of block comment.
    -> [String] -- ^ Alex declaration.
  lexBlockComment start end =
    [ unwords [ "-- Block comment", show start, show end ]
    , printRegAlex (mkRegMultilineComment start end) ++ " ;"
    , ""
    ]

  userDefTokenTypes :: [String]
  userDefTokenTypes = concat
    [ [ "-- token " ++ name
      , printRegAlex exp
      , "    { tok (eitherResIdent T_"  ++ name ++ ") }"
      , ""
      ]
    | (name, exp) <- tokenPragmas cf
    ]

  tokens = cfTokens cf

-- | Binary search tree.
data BTree a
  = N
  | B String a (BTree a) (BTree a)

instance Pretty a => Pretty (BTree a) where
  prettyPrec _  N          = text "N"
  prettyPrec n (B k v l r) = parensIf (n > 0) $
    hang ("b" <+> text (show k) <+> pretty v) 2 $ sep
      [ prettyPrec 1 l
      , prettyPrec 1 r
      ]

-- | Create a balanced search tree from a sorted list.
sorted2tree :: [(String,a)] -> BTree a
sorted2tree [] = N
sorted2tree xs = B x n (sorted2tree t1) (sorted2tree t2)
  where
  (t1, (x,n) : t2) = splitAt (length xs `div` 2) xs


-------------------------------------------------------------------
-- Inlined version of former @BNFC.Backend.Haskell.RegToAlex@.
-- Syntax has changed...
-------------------------------------------------------------------

-- modified from pretty-printer generated by the BNF converter

-- the top-level printing method
printRegAlex :: Reg -> String
printRegAlex = render' . prt 0

render' :: [String] -> String
render' = \case
    "["      : ts -> cons "["  $ render' ts
    "("      : ts -> cons "("  $ render' ts
    t  : "," : ts -> cons t    $ space "," $ render' ts
    t  : ")" : ts -> cons t    $ cons ")"  $ render' ts
    t  : "]" : ts -> cons t    $ cons "]"  $ render' ts
    t        : ts -> space t   $ render' ts
    _             -> ""
  where
  cons s t  = s ++ t
  space t s = if null s then t else t ++ " " ++ s

parenth :: [String] -> [String]
parenth ss = ["("] ++ ss ++ [")"]

-- the printer class does the job
class Print a where
  prt :: Int -> a -> [String]
  prtList :: [a] -> [String]
  prtList = concatMap (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ = \case
    '\n'             -> ["\\n"]
    '\t'             -> ["\\t"]
    '\r'             -> ["\\r"]
    '\f'             -> ["\\f"]
    c | isAlphaNum c -> [[c]]
    c | isPrint c    -> ['\\':[c]]  -- ['\'':c:'\'':[]] -- Does not work for )
    c                -> ['\\':show (ord c)]

  prtList = map (concat . prt 0)

prPrec :: Int -> Int -> [String] -> [String]
prPrec i j = if j<i then parenth else id

instance Print Identifier where
  prt _ (Identifier (_, i)) = [i]

instance Print Reg where
  prt i e = case e of
   RSeq reg0 reg    -> prPrec i 2 $ prt 2 reg0 ++ prt 3 reg
   RAlt reg0 reg    -> prPrec i 1 $ concat [prt 1 reg0 , ["|"] , prt 2 reg]
   RStar reg        -> prPrec i 3 $ prt 3 reg ++ ["*"]
   RPlus reg        -> prPrec i 3 $ prt 3 reg ++ ["+"]
   ROpt reg         -> prPrec i 3 $ prt 3 reg ++ ["?"]
   -- Atomic/parenthesized expressions
   RMinus reg0 reg  -> concat [ ["["], prt 2 reg0 , ["#"] , prt 2 reg, ["]"] ]
   REps             -> ["()"]
   RChar c          -> prt 0 c
   RAlts str        -> concat [["["],prt 0 str,["]"]]
   RSeqs str        -> prPrec i 2 $ prt 0 str
   RDigit           -> ["$d"]
   RLetter          -> ["$l"]
   RUpper           -> ["$c"]
   RLower           -> ["$s"]
   RAny             -> ["$u"]
