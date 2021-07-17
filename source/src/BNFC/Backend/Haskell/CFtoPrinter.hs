{-
    BNF Converter: Pretty-printer generator
    Copyright (C) 2004  Author:  Aarne Ranta

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BNFC.Backend.Haskell.CFtoPrinter (cf2Printer, compareRules) where

import Prelude hiding ((<>))

import BNFC.Backend.Haskell.Utils
import BNFC.CF
import BNFC.Options (TokenText(..))
import BNFC.Utils

import Data.Char     (toLower)
import Data.Either   (lefts)
import Data.Function (on)

import qualified Data.List as List

-- import Debug.Trace (trace)
import Text.PrettyPrint

-- AR 15/2/2002

type AbsMod = String

-- | Derive pretty-printer from a BNF grammar.
cf2Printer
  :: TokenText  -- ^ Are identifiers @ByteString@s or @Text@ rather than @String@s?  (Option @--bytestrings@ and @--text@)
  -> Bool    -- ^ Option @--functor@?
  -> Bool    -- ^ @--haskell-gadt@?
  -> String  -- ^ Name of created Haskell module.
  -> AbsMod  -- ^ Name of Haskell module for abstract syntax.
  -> CF      -- ^ Grammar.
  -> Doc
cf2Printer tokenText functor useGadt name absMod cf = vcat $ concat $
  -- Each of the following list entries is itself a list of Docs
  [ prologue tokenText useGadt name [ absMod | importAbsMod ] cf
  , integerRule absMod cf
  , doubleRule absMod cf
  , when (hasIdent cf) $ identRule absMod tokenText cf
  , concat [ ownPrintRule absMod tokenText cf own | (own,_) <- tokenPragmas cf ]
  , rules absMod functor cf
  ]
  where
  importAbsMod = not (null $ cf2data cf) || not (null $ specialCats cf)

-- | Lowercase Haskell identifiers imported from ''Prelude''.
lowerCaseImports :: [String]
lowerCaseImports =
  [ "all", "elem", "foldr", "id", "map", "null", "replicate", "shows", "span" ]

prologue :: TokenText -> Bool -> String -> [AbsMod] -> CF -> [Doc]
prologue tokenText useGadt name absMod cf = map text $ concat
  [ [ "{-# LANGUAGE CPP #-}"
    , "{-# LANGUAGE FlexibleInstances #-}"
    , "{-# LANGUAGE LambdaCase #-}"
    ]
  , [ "{-# LANGUAGE GADTs #-}"                | useGadt ]
  , [ "#if __GLASGOW_HASKELL__ <= 708"
    , "{-# LANGUAGE OverlappingInstances #-}"
    , "#endif"
    ]
  , [ ""
    , "{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}"
    , ""
    , "-- | Pretty-printer for " ++ takeWhile ('.' /=) name ++ "."
    , ""
    , "module " ++ name +++ "where"
    , ""
    , "import Prelude"
    , "  ( ($), (.)"
    , "  , Bool(..), (==), (<)"
    , "  , Int, Integer, Double, (+), (-), (*)"
    , "  , String, (++)"
    , "  , ShowS, showChar, showString"
    , "  , " ++ List.intercalate ", " lowerCaseImports
    , "  )"
    , "import Data.Char ( Char, isSpace )"
    ]
  , fmap ("import qualified " ++) absMod  -- At most 1.  (Unnecessary if Abs module is empty.)
  , when (hasTextualTokens cf) $ tokenTextImport tokenText
  , [ ""
    , "-- | The top-level printing method."
    , ""
    , "printTree :: Print a => a -> String"
    , "printTree = render . prt 0"
    , ""
    , "type Doc = [ShowS] -> [ShowS]"
    , ""
    , "doc :: ShowS -> Doc"
    , "doc = (:)"
    , ""
    , "render :: Doc -> String"
    , "render d = rend 0 False (map ($ \"\") $ d []) \"\""
    , "  where"
    , "  rend"
    , "    :: Int        -- ^ Indentation level."
    , "    -> Bool       -- ^ Pending indentation to be output before next character?"
    , "    -> [String]"
    , "    -> ShowS"
    , "  rend i p = \\case"
    , "      \"[\"      :ts -> char '[' . rend i False ts"
    , "      \"(\"      :ts -> char '(' . rend i False ts"
    , "      \"{\"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts"
    , "      \"}\" : \";\":ts -> onNewLine (i-1) p . showString \"};\" . new (i-1) ts"
    , "      \"}\"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts"
    , "      [\";\"]        -> char ';'"
    , "      \";\"      :ts -> char ';' . new i ts"
    , "      t  : ts@(s:_) | closingOrPunctuation s"
    , "                   -> pending . showString t . rend i False ts"
    , "      t        :ts -> pending . space t      . rend i False ts"
    , "      []           -> id"
    , "    where"
    , "    -- Output character after pending indentation."
    , "    char :: Char -> ShowS"
    , "    char c = pending . showChar c"
    , ""
    , "    -- Output pending indentation."
    , "    pending :: ShowS"
    , "    pending = if p then indent i else id"
    , ""
    , "  -- Indentation (spaces) for given indentation level."
    , "  indent :: Int -> ShowS"
    , "  indent i = replicateS (2*i) (showChar ' ')"
    , ""
    , "  -- Continue rendering in new line with new indentation."
    , "  new :: Int -> [String] -> ShowS"
    , "  new j ts = showChar '\\n' . rend j True ts"
    , ""
    , "  -- Make sure we are on a fresh line."
    , "  onNewLine :: Int -> Bool -> ShowS"
    , "  onNewLine i p = (if p then id else showChar '\\n') . indent i"
    , ""
    , "  -- Separate given string from following text by a space (if needed)."
    , "  space :: String -> ShowS"
    , "  space t s ="
    , "    case (all isSpace t', null spc, null rest) of"
    , "      (True , _   , True ) -> []              -- remove trailing space"
    , "      (False, _   , True ) -> t'              -- remove trailing space"
    , "      (False, True, False) -> t' ++ ' ' : s   -- add space if none"
    , "      _                    -> t' ++ s"
    , "    where"
    , "      t'          = showString t []"
    , "      (spc, rest) = span isSpace s"
    , ""
    , "  closingOrPunctuation :: String -> Bool"
    , "  closingOrPunctuation [c] = c `elem` closerOrPunct"
    , "  closingOrPunctuation _   = False"
    , ""
    , "  closerOrPunct :: String"
    , "  closerOrPunct = \")],;\""
    , ""
    , "parenth :: Doc -> Doc"
    , "parenth ss = doc (showChar '(') . ss . doc (showChar ')')"
    , ""
    , "concatS :: [ShowS] -> ShowS"
    , "concatS = foldr (.) id"
    , ""
    , "concatD :: [Doc] -> Doc"
    , "concatD = foldr (.) id"
    , ""
    , "replicateS :: Int -> ShowS -> ShowS"
    , "replicateS n f = concatS (replicate n f)"
    , ""
    , "-- | The printer class does the job."
    , ""
    , "class Print a where"
    , "  prt :: Int -> a -> Doc"
    , ""
    , "instance {-# OVERLAPPABLE #-} Print a => Print [a] where"
    , "  prt i = concatD . map (prt i)"
    , ""
    , "instance Print Char where"
    , "  prt _ c = doc (showChar '\\'' . mkEsc '\\'' c . showChar '\\'')"
    , ""
    ]
  , if haveListChar then
    [ "-- | No @instance 'Print' String@ because it would clash with the instance"
    , "--   for @[Char]@."
    ]
    else
    [ "instance Print String where"
    , "  prt _ = printString"
    , ""
    ]
  , [ "printString :: String -> Doc"
    , "printString s = doc (showChar '\"' . concatS (map (mkEsc '\"') s) . showChar '\"')"
    , ""
    , "mkEsc :: Char -> Char -> ShowS"
    , "mkEsc q = \\case"
    , "  s | s == q -> showChar '\\\\' . showChar s"
    , "  '\\\\' -> showString \"\\\\\\\\\""
    , "  '\\n' -> showString \"\\\\n\""
    , "  '\\t' -> showString \"\\\\t\""
    , "  s -> showChar s"
    , ""
    , "prPrec :: Int -> Int -> Doc -> Doc"
    , "prPrec i j = if j < i then parenth else id"
    , ""
    ]
  ]
  where
  haveListChar = not $ null $ rulesForCat cf $ ListCat $ TokenCat "Char"

-- | Printing instance for @Integer@, and possibly @[Integer]@.
integerRule :: AbsMod -> CF -> [Doc]
integerRule absMod cf = showsPrintRule absMod cf $ TokenCat catInteger

-- | Printing instance for @Double@, and possibly @[Double]@.
doubleRule :: AbsMod -> CF -> [Doc]
doubleRule absMod cf = showsPrintRule absMod cf $ TokenCat catDouble

showsPrintRule :: AbsMod -> CF -> Cat -> [Doc]
showsPrintRule absMod _cf t =
  [ hsep [ "instance Print" , text (qualifiedCat absMod t) , "where" ]
  , "  prt _ x = doc (shows x)"
  , ""
  ]

-- | Print category (data type name) qualified if user-defined.
--
qualifiedCat :: AbsMod -> Cat -> String
qualifiedCat absMod t = case t of
  TokenCat s
    | s `elem` baseTokenCatNames -> unqualified
    | otherwise                  -> qualified
  Cat{}       -> qualified
  ListCat c   -> concat [ "[", qualifiedCat absMod c, "]" ]
  CoercCat{}  -> impossible
  where
  unqualified = catToStr t
  qualified   = qualify absMod unqualified
  impossible  = error $ "impossible in Backend.Haskell.CFtoPrinter.qualifiedCat: " ++ catToStr t

qualify :: AbsMod -> String -> String
qualify absMod s = concat [ absMod, "." , s ]

-- | Printing instance for @Ident@, and possibly @[Ident]@.
identRule :: AbsMod -> TokenText -> CF -> [Doc]
identRule absMod tokenText cf = ownPrintRule absMod tokenText cf catIdent

-- | Printing identifiers and terminals.
ownPrintRule :: AbsMod -> TokenText -> CF -> TokenCat -> [Doc]
ownPrintRule absMod tokenText cf own =
  [ "instance Print" <+> q <+> "where"
  , "  prt _ (" <> q <+> posn <> ") = doc $ showString" <+> text (tokenTextUnpack tokenText "i")
  ]
 where
   q    = text $ qualifiedCat absMod $ TokenCat own
   posn = if isPositionCat cf own then "(_,i)" else "i"

-- | Printing rules for the AST nodes.
rules :: AbsMod -> Bool -> CF -> [Doc]
rules absMod functor cf = do
  (cat, xs :: [ (Fun, [Cat]) ]) <- cf2dataLists cf
  concat $
    [ case_fun absMod functor cf cat $ map (toArgs cat) xs
    , [ "" ]
    ]
  where
    toArgs :: Cat -> (Fun, [Cat]) -> Rule
    toArgs cat (cons, _) =
      case filter (\ (Rule f c _rhs _internal) ->
                        cons == funName f && cat == normCat (wpThing c))
                  (cfgRules cf)
      of
        (r : _) -> r
        -- 2018-01-14:  Currently, there can be overlapping rules like
        --   Foo. Bar ::= "foo" ;
        --   Foo. Bar ::= "bar" ;
        -- Of course, this will generate an arbitary printer for @Foo@.
        [] -> error $ "CFToPrinter.rules: no rhs found for: " ++ cons ++ ". " ++ catToStr cat ++ " ::= ?"

-- |
-- >>> vcat $ case_fun "Abs" False undefined (Cat "A") [ (npRule "AA" (Cat "AB") [Right "xxx"]) Parsable ]
-- instance Print Abs.A where
--   prt i = \case
--     Abs.AA -> prPrec i 0 (concatD [doc (showString "xxx")])
case_fun :: AbsMod -> Bool -> CF -> Cat -> [Rule] -> [Doc]
case_fun absMod functor cf cat rules =
  -- trace ("case_fun: cat   = " ++ catToStr cat) $
  -- trace ("case_fun: rules = " ++ show rules ) $
  [ "instance Print" <+> type_ <+> "where"
  , nest 2 $ vcat $

      -- Special printing of lists (precedence changes concrete syntax!)
      if isList cat then
        map mkPrtListCase $ List.sortBy compareRules $ rulesForNormalizedCat cf cat

      -- Ordinary category
      else
        [ "prt i = \\case"
        , nest 2 $ vcat $ map (mkPrintCase absMod functor) rules
        ]
  ]
  where
    type_
     | functor   = case cat of
         ListCat{}  -> type' cat
         _ -> parens $ type' cat
     | otherwise = text (qualifiedCat absMod cat)
    type' = \case
      ListCat c    -> "[" <> type' c <> "]"
      c@TokenCat{} -> text (qualifiedCat absMod c)
      c            -> text (qualifiedCat absMod c) <> "' a"

-- | When writing the Print instance for a category (in case_fun), we have
-- a different case for each constructor for this category.
--
-- >>> mkPrintCase "Abs" False (npRule "AA" (Cat "A") [Right "xxx"] Parsable)
-- Abs.AA -> prPrec i 0 (concatD [doc (showString "xxx")])
--
-- Coercion levels are passed to @prPrec@.
--
-- >>> mkPrintCase "Abs" False (npRule "EInt" (CoercCat "Expr" 2) [Left (TokenCat "Integer")] Parsable)
-- Abs.EInt n -> prPrec i 2 (concatD [prt 0 n])
--
-- >>> mkPrintCase "Abs" False (npRule "EPlus" (CoercCat "Expr" 1) [Left (Cat "Expr"), Right "+", Left (Cat "Expr")] Parsable)
-- Abs.EPlus expr1 expr2 -> prPrec i 1 (concatD [prt 0 expr1, doc (showString "+"), prt 0 expr2])
--
-- If the AST is a functor, ignore first argument.
--
-- >>> mkPrintCase "Abs" True (npRule "EInt" (CoercCat "Expr" 2) [Left (TokenCat "Integer")] Parsable)
-- Abs.EInt _ n -> prPrec i 2 (concatD [prt 0 n])
--
-- Skip internal categories.
--
-- >>> mkPrintCase "Abs" True $ npRule "EInternal" (Cat "Expr") [Left (Cat "Expr")] Internal
-- Abs.EInternal _ expr -> prPrec i 0 (concatD [prt 0 expr])
--
mkPrintCase :: AbsMod -> Bool -> Rule -> Doc
mkPrintCase absMod functor (Rule f cat rhs _internal) =
    pat <+> "->"
    <+> "prPrec i" <+> integer (precCat $ wpThing cat) <+> parens (mkRhs (map render variables) rhs)
  where
    pat :: Doc
    pat
      | isNilFun  f = text "[]"
      | isOneFun  f = text "[" <+> head variables <+> "]"
      | isConsFun f = hsep $ List.intersperse (text ":") variables
      | otherwise   = text (qualify absMod $ funName f) <+> (if functor then "_" else empty) <+> hsep variables
    -- Creating variables names used in pattern matching. In addition to
    -- haskell's reserved words, `e` and `i` are used in the printing function
    -- and should be avoided.
    -- #337: `prt` as well, and some more entirely lowercase ones.
    avoid = concat
      [ [ "e", "i", "doc", "prt" ]  -- don't need mixed-case ones: "concatD", "prPrec", "showString"
      , lowerCaseImports
      , hsReservedWords
      ]
    names = map var (lefts rhs)
    variables :: [Doc]
    variables = map text $ mkNames avoid LowerCase names
    var (ListCat c)  = var c ++ "s"
    var (TokenCat "Ident")   = "id"
    var (TokenCat "Integer") = "n"
    var (TokenCat "String")  = "str"
    var (TokenCat "Char")    = "c"
    var (TokenCat "Double")  = "d"
    var xs = map toLower $ catToStr xs

-- | Pattern match on the list constructor and the coercion level
--
-- >>> mkPrtListCase (npRule "[]" (ListCat (Cat "Foo")) [] Parsable)
-- prt _ [] = concatD []
--
-- >>> mkPrtListCase (npRule "(:[])" (ListCat (Cat "Foo")) [Left (Cat "FOO")] Parsable)
-- prt _ [x] = concatD [prt 0 x]
--
-- >>> mkPrtListCase (npRule "(:)" (ListCat (Cat "Foo")) [Left (Cat "Foo"), Left (ListCat (Cat "Foo"))] Parsable)
-- prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]
--
-- >>> mkPrtListCase (npRule "[]" (ListCat (CoercCat "Foo" 2)) [] Parsable)
-- prt 2 [] = concatD []
--
-- >>> mkPrtListCase (npRule "(:[])" (ListCat (CoercCat "Foo" 2)) [Left (CoercCat "Foo" 2)] Parsable)
-- prt 2 [x] = concatD [prt 2 x]
--
-- >>> mkPrtListCase (npRule "(:)" (ListCat (CoercCat "Foo" 2)) [Left (CoercCat "Foo" 2), Left (ListCat (CoercCat "Foo" 2))] Parsable)
-- prt 2 (x:xs) = concatD [prt 2 x, prt 2 xs]
--
mkPrtListCase :: Rule -> Doc
mkPrtListCase (Rule f (WithPosition _ (ListCat c)) rhs _internal)
  | isNilFun f = "prt" <+> precPattern <+> "[]" <+> "=" <+> body
  | isOneFun f = "prt" <+> precPattern <+> "[x]" <+> "=" <+> body
  | isConsFun f = "prt" <+> precPattern <+> "(x:xs)" <+> "=" <+> body
  | otherwise = empty -- (++) constructor
  where
    precPattern = case precCat c of 0 -> "_" ; p -> integer p
    body = mkRhs ["x", "xs"] rhs
mkPrtListCase _ = error "mkPrtListCase undefined for non-list categories"


-- | Define an ordering on lists' rules with the following properties:
--
-- - rules with a higher coercion level should come first, i.e. the rules for
--   [Foo3] are before rules for [Foo1] and they are both lower than rules for
--   [Foo].
--
-- - [] < [_] < _:_
--
-- This is desiged to correctly order the rules in the prt function for lists so that
-- the pattern matching works as expectd.
--
-- >>> compareRules (npRule "[]" (ListCat (CoercCat "Foo" 3)) [] Parsable) (npRule "[]" (ListCat (CoercCat "Foo" 1)) [] Parsable)
-- LT
--
-- >>> compareRules (npRule "[]" (ListCat (CoercCat "Foo" 3)) [] Parsable) (npRule "[]" (ListCat (Cat "Foo")) [] Parsable)
-- LT
--
-- >>> compareRules (npRule "[]" (ListCat (Cat "Foo")) [] Parsable) (npRule "(:[])" (ListCat (Cat "Foo")) [] Parsable)
-- LT
--
-- >>> compareRules (npRule "(:[])" (ListCat (Cat "Foo")) [] Parsable) (npRule "(:)" (ListCat (Cat "Foo")) [] Parsable)
-- LT
--
compareRules :: IsFun f => Rul f -> Rul f -> Ordering
compareRules r1 r2
  | precRule r1 > precRule r2 = LT
  | precRule r1 < precRule r2 = GT
  | otherwise = (compareFunNames `on` (funName . funRule)) r1 r2

compareFunNames :: String -> String -> Ordering
compareFunNames = curry $ \case
  ("[]"    , "[]"   ) -> EQ
  ("[]"    , _      ) -> LT
  ("(:[])" , "[]"   ) -> GT
  ("(:[])" , "(:[])") -> EQ
  ("(:[])" , "(:)"  ) -> LT
  ("(:)"   , "(:)"  ) -> EQ
  ("(:)"   , _      ) -> GT
  (_       , _      ) -> EQ


-- |
--
-- >>> mkRhs ["expr1", "n", "expr2"] [Left (Cat "Expr"), Right "-", Left (TokenCat "Integer"), Left (Cat "Expr")]
-- concatD [prt 0 expr1, doc (showString "-"), prt 0 n, prt 0 expr2]
--
-- Coercions on the right hand side should be passed to prt:
--
-- >>> mkRhs ["expr1"] [Left (CoercCat "Expr" 2)]
-- concatD [prt 2 expr1]
--
-- >>> mkRhs ["expr2s"] [Left (ListCat (CoercCat "Expr" 2))]
-- concatD [prt 2 expr2s]
--
mkRhs :: [String] -> [Either Cat String] -> Doc
mkRhs args its =
  "concatD" <+> brackets (hsep (punctuate "," (mk args its)))
  where
  mk (arg:args) (Left c  : items)    = (prt c <+> text arg) : mk args items
  mk args       (Right s : items)    = ("doc (showString" <+> text (show s) <> ")") : mk args items
  mk _          _                    = []
  prt (TokenCat "String") = "printString"
  prt c = "prt" <+> integer (precCat c)
