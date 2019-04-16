{- |  Agda backend.
Generate bindings to Haskell data types for use in Agda.

Example for abstract syntax generated in Haskell backend:
@@
  newtype Ident = Ident String deriving (Eq, Ord, Show, Read)

  data Def = DFun Type Ident [Arg] [Stm]
    deriving (Eq, Ord, Show, Read)

  data Arg = ADecl Type Ident
    deriving (Eq, Ord, Show, Read)

  data Stm
      = SExp Exp
      | SInit Type Ident Exp
      | SBlock [Stm]
      | SIfElse Exp Stm Stm
    deriving (Eq, Ord, Show, Read)

  data Type = Type_bool | Type_int | Type_double | Type_void
    deriving (Eq, Ord, Show, Read)
@@
This should be accompanied by the following Agda code:
@@
  module <mod> where

  {-# FOREIGN GHC import qualified Data.Text #-}
  {-# FOREIGN GHC import CPP.Abs #-}
  {-# FOREIGN GHC import CPP.Print #-}

  data Ident : Set where
    ident : List Char → Ident

  {-# COMPILE GHC Ident = data Ident (Ident) #-}

  data Def : Set where
    dFun : (t : Type) (x : Ident) (as : List Arg) (ss : List Stm) → Def

  {-# COMPILE GHC Def = data Def (DFun) #-}

  data Arg : Set where
    aDecl : (t : Type) (x : Ident) → Arg

  {-# COMPILE GHC Arg = data Arg (ADecl) #-}

  data Stm : Set where
    sExp : (e : Exp) → Stm
    sInit : (t : Type) (x : Ident) (e : Exp) → Stm
    sBlock : (ss : List Stm) → Stm
    sIfElse : (e : Exp) (s s' : Stm) → Stm

  {-# COMPILE GHC Stm = data Stm
    ( SExp
    | SInit
    | SBlock
    | SIfElse
    ) #-}

  data Type : Set where
    typeBool typeInt typeDouble typeVoid : Type

  {-# COMPILE GHC Type = data Type
    ( Type_bool
    | Type_int
    | Type_double
    | Type_void
    ) #-}

  -- Binding the BNFC pretty printer.

  printIdent  : Ident → String
  printIdent (ident s) = String.fromList s

  postulate
    printType    : Type    → String
    printExp     : Exp     → String
    printStm     : Stm     → String
    printArg     : Arg     → String
    printDef     : Def     → String
    printProgram : Program → String

  {-# COMPILE GHC printType    = \ t -> Data.Text.pack (printTree (t :: Type)) #-}
  {-# COMPILE GHC printExp     = \ e -> Data.Text.pack (printTree (e :: Exp))  #-}
  {-# COMPILE GHC printStm     = \ s -> Data.Text.pack (printTree (s :: Stm))  #-}
  {-# COMPILE GHC printArg     = \ a -> Data.Text.pack (printTree (a :: Arg))  #-}
  {-# COMPILE GHC printDef     = \ d -> Data.Text.pack (printTree (d :: Def))  #-}
  {-# COMPILE GHC printProgram = \ p -> Data.Text.pack (printTree (p :: Program)) #-}
@@
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module BNFC.Backend.Agda (makeAgda) where

import Prelude'
import Control.Monad.State
import Data.Char
import Data.Function (on)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint

import BNFC.CF
import BNFC.Utils                  (NameStyle(..), mkName, replace)
import BNFC.Backend.Base           (Backend, mkfile)
import BNFC.Options                (SharedOptions)
import BNFC.Backend.Haskell.HsOpts (agdaFile, agdaFileM, absFileM, printerFileM)

-- | How to print the types of constructors in Agda?

data ConstructorStyle
  = UnnamedArg  -- ^ Simply typed, like @E → S → S → S@.
  | NamedArg    -- ^ Dependently typed, like @(e : E) (s₁ s₂ : S) → S@.

-- | Entry-point for Agda backend.

makeAgda
  :: String         -- ^ Current time.
  -> SharedOptions  -- ^ Options.
  -> CF             -- ^ Grammar.
  -> Backend
makeAgda time opts cf = do
  mkfile (agdaFile opts) $
    cf2Agda time (agdaFileM opts) (absFileM opts) (printerFileM opts) cf

-- | Generate AST bindings for Agda.

cf2Agda
  :: String  -- ^ Current time.
  -> String  -- ^ Module name.
  -> String  -- ^ Haskell Abs module name.
  -> String  -- ^ Haskell Print module name.
  -> CF      -- ^ Grammar.
  -> String
cf2Agda time mod amod pmod cf = render . vsep $
  [ preamble time
  , hsep [ "module", text mod, "where" ]
  , imports
  , importPragmas amod pmod
  , allTokenCats prToken tcats
  , absyn NamedArg dats
  , "-- Binding the BNFC pretty printer"
  , allTokenCats printToken tcats
  , printer cats
  , empty -- Make sure we terminate the file with a new line.
  ]
  where
  -- The grammar categories:
  dats = cf2data cf
     -- getAbstractSyntax also includes list categories, which isn't what we need
  cats = map fst dats
  -- The token categories:
  tcats = specialCats cf

-- We prefix the Agda types with "#" to not conflict with user-provided nonterminals.
arrow, charT, intT, listT, stringT, stringFromListT :: Doc
arrow = "→"
charT           = "#Char"
intT            = "Integer"  -- This is the BNFC name for token type Integer!
doubleT         = "Double"   -- This is the BNFC name for token type Double!
listT           = "#List"
stringT         = "#String"
stringFromListT = "#stringFromList"

-- | Hack to insert blank lines.

($++$) :: Doc -> Doc -> Doc
d $++$ d'
  | isEmpty d  = d'
  | isEmpty d' = d
  | otherwise  = d $+$ "" $+$ d'

-- | Separate vertically by blank lines.

vsep :: [Doc] -> Doc
vsep [] = empty
vsep ds = foldr1 ($++$) ds

-- | Preamble: introductory comments.

preamble :: String -> Doc
preamble time = vcat $
  [ "-- Agda bindings for the Haskell abstract syntax data types."
  , "-- Generated by BNFC at" <+> text time <> "."
  ]

-- -- | Generate Agda module header.
-- --   We parametrized the module over the implementation List, Char, String, and stringFromList.
-- --
-- -- >>> header "AST"
-- -- module AST
-- --   (#List : Set → Set)
-- --   (#Char : Set)
-- --   (#String : Set)
-- --   (#stringFromList : #List #Char → #String)
-- --   where
-- --
-- header :: String -> Doc
-- header mod = vcat . concat $
--   [ [ "module" <+> text mod ]
--   , map (nest 2 . parens) parameters
--   , [ nest 2 $ "where" ]
--   ]
--   where
--   parameters :: [Doc]
--   parameters =
--     [ hsep [ listT, colon, setT, arrow, setT ]
--     , hsep [ charT, colon, setT ]
--     , hsep [ stringT, colon, setT ]
--     , hsep [ stringFromListT, colon, listT, charT, arrow, stringT ]
--     ]

-- | Import statements.

imports :: Doc
imports = vcat . map prettyImport $
  [ ("Agda.Builtin.Char",   [("Char", charT)])
  , ("Agda.Builtin.Float",  [("Float", doubleT)])
  , ("Agda.Builtin.Int",    [("Int", intT)])
  , ("Agda.Builtin.List",   [("List", listT)])
  , ("Agda.Builtin.String", [("String", stringT), ("primStringFromList", stringFromListT) ])
  ]
  where
  prettyImport :: (String, [(String, Doc)]) -> Doc
  prettyImport (m, ren) = prettyList pre lparen rparen semi $
    map (\ (x, d) -> hsep [text x, "to", d ]) ren
    where
    pre = hsep [ "open", "import", text m, "using", "()", "renaming" ]

-- | Import pragmas.
--
-- >>> importPragmas "Foo.Abs" "Foo.Print"
-- {-# FOREIGN GHC import qualified Data.Text #-}
-- {-# FOREIGN GHC import Foo.Abs #-}
-- {-# FOREIGN GHC import Foo.Print #-}
--
importPragmas
  :: String  -- ^ Haskell Abs module
  -> String  -- ^ Haskell Print module
  -> Doc
importPragmas amod pmod = vcat $ map imp [ "qualified Data.Text" , amod, pmod ]
  where
  imp s = hsep [ "{-#", "FOREIGN", "GHC", "import", text s, "#-}" ]

-- * Bindings for the AST.

-- | Pretty-print types for token types similar to @Ident@.

prToken :: String -> Doc
prToken t =
  prettyData UnnamedArg t [(agdaLower t, [ListCat (Cat "#Char")])]
  $++$
  pragmaData t [(t, [])]

-- | Pretty-print abstract syntax definition in Agda syntax.
--
--   We print this as one big mutual block rather than doing a
--   strongly-connected component analysis and topological
--   sort by dependency order.
--
absyn :: ConstructorStyle -> [Data] -> Doc
absyn style = vsep . ("mutual" :) . concatMap (map (nest 2) . prData style)

-- | Pretty-print Agda data types and pragmas for AST.
--
-- >>> vsep $ prData UnnamedArg (Cat "Nat", [ ("zero", []), ("suc", [Cat "Nat"]) ])
-- data Nat : Set where
--   zero : Nat
--   suc : Nat → Nat
-- <BLANKLINE>
-- {-# COMPILE GHC Nat = data Nat
--   ( zero
--   | suc
--   ) #-}
--
-- We return a list of 'Doc' rather than a single 'Doc' since want
-- to intersperse empty lines and indent it later.
-- If we intersperse the empty line(s) here to get a single 'Doc',
-- we will produce whitespace lines after applying 'nest'.
-- This is a bit of a design problem of the pretty print library:
-- there is no native concept of a blank line; @text ""@ is a bad hack.
--
prData :: ConstructorStyle -> Data -> [Doc]
prData style (Cat d, cs) = [ prettyData style d cs , pragmaData d cs ]
prData _     (c    , _ ) = error $ "prData: unexpected category " ++ show c

-- | Pretty-print AST definition in Agda syntax.
--
-- >>> prettyData UnnamedArg "Nat" [ ("zero", []), ("suc", [Cat "Nat"]) ]
-- data Nat : Set where
--   zero : Nat
--   suc : Nat → Nat
-- >>> let stm = Cat "Stm" in prettyData UnnamedArg "Stm" [ ("block", [ListCat stm]), ("while", [Cat "Exp", stm]) ]
-- data Stm : Set where
--   block : #List Stm → Stm
--   while : Exp → Stm → Stm
--
prettyData :: ConstructorStyle -> String -> [(Fun, [Cat])] -> Doc
prettyData style d cs = vcat $
  [ hsep [ "data", text d, colon, "Set", "where" ] ] ++
  map (nest 2 . prettyConstructor style d) cs

-- | Generate pragmas to bind Haskell AST to Agda.
--
-- >>> pragmaData "Empty" []
-- {-# COMPILE GHC Empty = data Empty () #-}
--
-- >>> pragmaData "Nat" [ ("zero", []), ("suc", [Cat "Nat"]) ]
-- {-# COMPILE GHC Nat = data Nat
--   ( zero
--   | suc
--   ) #-}
pragmaData :: String -> [(Fun, [Cat])] -> Doc
pragmaData d cs = prettyList pre lparen (rparen <+> "#-}") "|" $
  map (prettyFun . fst) cs
  where
  pre = hsep [ "{-#", "COMPILE", "GHC", text d, equals, "data", text d ]

-- | Pretty-print since rule as Agda constructor declaration.
--
-- >>> prettyConstructor UnnamedArg "D" ("c", [Cat "A", Cat "B", Cat "C"])
-- c : A → B → C → D
-- >>> prettyConstructor undefined  "D" ("c", [])
-- c : D
-- >>> prettyConstructor NamedArg "Stm" ("SIf", map Cat ["Exp", "Stm", "Stm"])
-- sIf : (e : Exp) (s₁ s₂ : Stm) → Stm
--
prettyConstructor :: ConstructorStyle -> String -> (Fun,[Cat]) -> Doc
prettyConstructor _style d (c, []) = hsep $
  [ prettyCon c
  , colon
  , text d
  ]
prettyConstructor style d (c, as) = hsep $
  [ prettyCon c
  , colon
  , prettyConstructorArgs style as
  , arrow
  , text d
  ]

-- | Print the constructor argument telescope.
--
-- >>> prettyConstructorArgs UnnamedArg [Cat "A", Cat "B", Cat "C"]
-- A → B → C
-- >>> prettyConstructorArgs NamedArg (map Cat ["Exp", "Stm", "Stm"])
-- (e : Exp) (s₁ s₂ : Stm)
--
prettyConstructorArgs :: ConstructorStyle -> [Cat] -> Doc
prettyConstructorArgs style as =
  case style of
    UnnamedArg -> hsep $ List.intersperse arrow ts
    NamedArg   -> hsep $ map (\(xs,t) -> parens (hsep [hsep xs, colon, t])) tel
  where
  ts  = map prettyCat as
  ns  = map (text . subscript) $ numberUniquely $ map nameSuggestion as
  tel = aggregate $ zip ns ts
  subscript (m, s) = maybe s (\ i -> s ++ [chr (ord '₀' + i)]) m
  -- Aggregate consecutive arguments of the same type.
  aggregate :: Eq b => [(a,b)] -> [([a],b)]
  aggregate = map (\ xts -> (map fst xts, snd (head xts))) . List.groupBy ((==) `on` snd)

-- | Suggest the name of a bound variable of the given category.
--
-- >>> map nameSuggestion [ ListCat (Cat "Stm"), TokenCat "Var", Cat "Exp" ]
-- ["ss","x","e"]
--
nameSuggestion :: Cat -> String
nameSuggestion = \case
  ListCat c     -> nameSuggestion c ++ "s"
  CoercCat d _  -> nameFor d
  Cat d         -> nameFor d
  TokenCat{}    -> "x"

-- | Suggest the name of a bound variable of the given base category.
--
-- >>> map nameFor ["Stm","ABC","#Char"]
-- ["s","a","c"]
--
nameFor :: String -> String
nameFor d = [ toLower $ head $ dropWhile (== '#') d ]

-- | Number duplicate elements in a list consecutively, starting with 1.
--
-- >>> numberUniquely ["a", "b", "a", "a", "c", "b"]
-- [(Just 1,"a"),(Just 1,"b"),(Just 2,"a"),(Just 3,"a"),(Nothing,"c"),(Just 2,"b")]
--
numberUniquely :: forall a. Ord a => [a] -> [(Maybe Int, a)]
numberUniquely as = mapM step as `evalState` Map.empty
  where
  -- First pass: determine frequency of each element.
  counts :: Frequency a
  counts = foldl (flip incr) Map.empty as
  -- Second pass: consecutively number elements with frequency > 1.
  step :: a -> State (Frequency a) (Maybe Int, a)
  step a = do
    -- If the element has a unique occurrence, we do not need to number it.
    let n = Map.findWithDefault (error "numberUniquelyWith") a counts
    if n == 1 then return (Nothing, a) else do
      -- Otherwise, increase the counter for that element and number it
      -- with the new value.
      modify $ incr a
      (,a) . Map.lookup a <$> get

-- | A frequency map.
--
--   NB: this type synonym should be local to 'numberUniquely', but
--   Haskell lacks local type synonyms.
--   https://gitlab.haskell.org/ghc/ghc/issues/4020
type Frequency a = Map a Int

-- | Increase the frequency of the given key.
incr :: Ord a => a -> Frequency a -> Frequency a
incr = Map.alter $ maybe (Just 1) (Just . succ)

-- * Generate bindings for the pretty printer

-- | Generate Agda code to print tokens.
--
-- >>> printToken "Ident"
-- printIdent : Ident → #String
-- printIdent (ident s) = #stringFromList s
--
printToken :: String -> Doc
printToken t = vcat
  [ hsep [ f, colon, text t, arrow, stringT ]
  , hsep [ f, lparen <> c <+> "s" <> rparen, equals, stringFromListT, "s" ]
  ]
  where
  f = text $ "print" ++ t
  c = text $ agdaLower t

-- | Generate Agda bindings to printers for AST.
--
-- >>> printer $ map Cat [ "Exp", "Stm" ]
-- postulate
--   printExp : Exp → #String
--   printStm : Stm → #String
-- <BLANKLINE>
-- {-# COMPILE GHC printExp = \ e -> Data.Text.pack (printTree (e :: Exp)) #-}
-- {-# COMPILE GHC printStm = \ s -> Data.Text.pack (printTree (s :: Stm)) #-}
--
printer :: [Cat] -> Doc
printer cats =
  vcat ("postulate" : map (nest 2 . prettyTySig) ts)
  $++$
  vcat (map pragmaBind ts)
  where
  ts = map (\ (Cat x) -> x) cats
  prettyTySig x = hsep [ text ("print" ++ x), colon, text x, arrow, stringT ]
  pragmaBind  x = hsep
    [ "{-#", "COMPILE", "GHC", text ("print" ++ x), equals, "\\", y, "->"
    , "Data.Text.pack", parens ("printTree" <+> parens (y <+> "::" <+> text x)), "#-}"
    ]
    where
    y = text $ nameFor x

-- * Auxiliary functions

-- | Concatenate documents created from token categories,
--   separated by blank lines.
--
-- >>> allTokenCats text $ map TokenCat ["T", "U"]
-- T
-- <BLANKLINE>
-- U
allTokenCats :: (String -> Doc) -> [Cat] -> Doc
allTokenCats f = vsep . map (\ (TokenCat t) -> f t)

-- | Pretty-print a rule name for Haskell.
prettyFun :: Fun -> Doc
prettyFun = text

-- | Pretty-print a rule name for Agda.
prettyCon :: Fun -> Doc
prettyCon = text . agdaLower

-- | Turn identifier to non-capital identifier.
--   Needed, since in Agda a constructor cannot overload a data type
--   with the same name.
--
-- >>> map agdaLower ["SFun","foo","ABC","HelloWorld","module","Type_int"]
-- ["sFun","foo","aBC","helloWorld","module'","typeInt"]
--
agdaLower :: String -> String
agdaLower = replace '_' '\'' . mkName agdaKeywords MixedCase

-- | A list of Agda keywords that would clash with generated names.
agdaKeywords :: [String]
agdaKeywords = words "abstract codata coinductive constructor data do eta-equality field forall hiding import in inductive infix infixl infixr instance let macro module mutual no-eta-equality open overlap pattern postulate primitive private public quote quoteContext quoteGoal quoteTerm record renaming rewrite Set syntax tactic unquote unquoteDecl unquoteDef using where with"

-- | Pretty-print a category as Agda type.
prettyCat :: Cat -> Doc
prettyCat = \case
  Cat s        -> text s
  TokenCat s   -> text s
  CoercCat s _ -> text s
  ListCat c    -> listT <+> parensIf (compositeCat c) (prettyCat c)
  InternalCat  -> error "prettyCat: unexpected case InternalCat"

-- | Is the Agda type corresponding to 'Cat' composite (or atomic)?
compositeCat :: Cat -> Bool
compositeCat = \case
  ListCat{} -> True
  _         -> False

parensIf :: Bool -> Doc -> Doc
parensIf = \case
  True  -> parens
  False -> id

-- | Print a list of 0-1 elements on the same line as some preamble
--   and from 2 elements on the following lines, indented.
--
-- >>> prettyList ("foo" <+> equals) lbrack rbrack comma []
-- foo = []
-- >>> prettyList ("foo" <+> equals) lbrack rbrack comma [ "a" ]
-- foo = [a]
-- >>> prettyList ("foo" <+> equals) lbrack rbrack comma [ "a", "b" ]
-- foo =
--   [ a
--   , b
--   ]
prettyList
  :: Doc   -- ^ Preamble.
  -> Doc   -- ^ Left parenthesis.
  -> Doc   -- ^ Right parenthesis.
  -> Doc   -- ^ Separator (usually not including spaces).
  -> [Doc] -- ^ List item.
  -> Doc
prettyList pre lpar rpar sepa = \case
  []     -> pre <+> lpar <> rpar
  [d]    -> pre <+> lpar <> d <> rpar
  (d:ds) -> vcat . (pre :) . map (nest 2) . concat $
    [ [ lpar <+> d ]
    , map (sepa <+>) ds
    , [ rpar ]
    ]
