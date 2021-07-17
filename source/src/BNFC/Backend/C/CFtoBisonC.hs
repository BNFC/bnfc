{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-
    BNF Converter: C Bison generator
    Copyright (C) 2004  Author:  Michael Pellauer

    Description   : This module generates the Bison input file.
                    Note that because of the way bison stores results
                    the programmer can increase performance by limiting
                    the number of entry points in their grammar.

    Author        : Michael Pellauer
    Created       : 6 August, 2003
-}

module BNFC.Backend.C.CFtoBisonC
  ( cf2Bison
  , resultName, typeName, varName
  , specialToks, startSymbol
  , unionBuiltinTokens
  )
  where

import Prelude hiding ((<>))

import Data.Char       ( toLower, isUpper )
import Data.Foldable   ( toList )
import Data.List       ( intercalate, nub )
import qualified Data.Map as Map
import System.FilePath ( (<.>) )

import BNFC.CF
import BNFC.Backend.Common.NamedVariables hiding (varName)
import BNFC.Backend.C.CFtoFlexC (ParserMode(..), cParser, stlParser, parserHExt, parserName, parserPackage)
import BNFC.Backend.CPP.Naming
import BNFC.Backend.CPP.STL.STLUtils
import BNFC.Options (RecordPositions(..), InPackage)
import BNFC.PrettyPrint
import BNFC.Utils ((+++), table, applyWhen, for, unless, when, whenJust)

--This follows the basic structure of CFtoHappy.

-- Type declarations
type Rules       = [(NonTerminal,[(Pattern,Action)])]
type Pattern     = String
type Action      = String
type MetaVar     = String

--The environment comes from the CFtoFlex
cf2Bison :: RecordPositions -> ParserMode -> CF -> SymMap -> String
cf2Bison rp mode cf env = unlines
    [ header mode cf
    , render $ union mode $ posCats ++ allParserCatsNorm cf
    , ""
    , unionDependentCode mode
    , unlines $ table " " $ concat
      [ [ ["%token", "_ERROR_" ] ]
      , tokens (map fst $ tokenPragmas cf) env
      , specialToks cf
      ]
    , declarations mode cf
    , startSymbol cf
    , ""
    , "%%"
    , ""
    , prRules $ rulesForBison rp mode cf env
    , "%%"
    , ""
    , nsStart inPackage
    , entryCode mode cf
    , nsEnd inPackage
    ]
  where
  inPackage = parserPackage mode
  posCats
    | stlParser mode = map TokenCat $ positionCats cf
    | otherwise      = []

positionCats :: CF -> [String]
positionCats cf = [ wpThing name | TokenReg name True _ <- cfgPragmas cf ]

header :: ParserMode -> CF -> String
header mode cf = unlines $ concat
  [ [ "/* -*- c -*- Parser definition to be used with Bison. */"
    , ""
    , "/* Generate header file for lexer. */"
    , "%defines \"" ++ ("Bison" <.> h) ++ "\""
    ]
  , whenJust (parserPackage mode) $ \ ns ->
    [ "%name-prefix = \"" ++ ns ++ "\""
    , "  /* From Bison 2.6: %define api.prefix {" ++ ns ++ "} */"
    ]
  , [ ""
    , "/* Reentrant parser */"
    , "%pure_parser"
    , "  /* From Bison 2.3b (2008): %define api.pure full */"
         -- The flag %pure_parser is deprecated with a warning since Bison 3.4,
         -- but older Bisons like 2.3 (2006, shipped with macOS) don't recognize
         -- %define api.pure full
    , "%lex-param   { yyscan_t scanner }"
    , "%parse-param { yyscan_t scanner }"
    , ""
    , concat [ "/* Turn on line/column tracking in the ", name, "lloc structure: */" ]
    , "%locations"
    , ""
    , "/* Argument to the parser to be filled with the parsed tree. */"
    , "%parse-param { YYSTYPE *result }"
    , ""
    , "%{"
    , "/* Begin C preamble code */"
    , ""
    , "#include <stdlib.h>"
    , "#include <stdio.h>"
    , "#include <string.h>"
    , "#include \"" ++ ("Absyn" <.> h) ++ "\""
    , ""
    , "#define YYMAXDEPTH 10000000"  -- default maximum stack size is 10000, but right-recursion needs O(n) stack
    , ""
    , "/* The type yyscan_t is defined by flex, but we need it in the parser already. */"
    , "#ifndef YY_TYPEDEF_YY_SCANNER_T"
    , "#define YY_TYPEDEF_YY_SCANNER_T"
    , "typedef void* yyscan_t;"
    , "#endif"
    , ""
    -- , "typedef struct " ++ name ++ "_buffer_state *YY_BUFFER_STATE;"
    , "typedef struct yy_buffer_state *YY_BUFFER_STATE;"
    , "extern YY_BUFFER_STATE " ++ name ++ "_scan_string(const char *str, yyscan_t scanner);"
    , "extern void " ++ name ++ "_delete_buffer(YY_BUFFER_STATE buf, yyscan_t scanner);"
    , ""
    , "extern void " ++ name ++ "lex_destroy(yyscan_t scanner);"
    , "extern char* " ++ name ++ "get_text(yyscan_t scanner);"
    , ""
    , "extern yyscan_t " ++ name ++ "_initialize_lexer(FILE * inp);"
    , ""
    ]
  , unless (stlParser mode)
    [ "/* List reversal functions. */"
    , concatMap (reverseList mode) $ filter isList $ allParserCatsNorm cf
    ]
  , [ "/* End C preamble code */"
    , "%}"
    ]
  ]
  where
  h    = parserHExt mode
  name = parserName mode

-- | Code that needs the @YYSTYPE@ defined by the @%union@ pragma.
--
unionDependentCode :: ParserMode -> String
unionDependentCode mode = unlines
  [ "%{"
  , errorHandler name
  , "int yyparse(yyscan_t scanner, YYSTYPE *result);"
  , ""
  , "extern int yylex(YYSTYPE *lvalp, YYLTYPE *llocp, yyscan_t scanner);"
  , "%}"
  ]
  where
  name = parserName mode

errorHandler :: String -> String
errorHandler name = unlines
  [ "void yyerror(YYLTYPE *loc, yyscan_t scanner, YYSTYPE *result, const char *msg)"
  , "{"
  , "  fprintf(stderr, \"error: %d,%d: %s at %s\\n\","
  , "    loc->first_line, loc->first_column, msg, " ++ name ++ "get_text(scanner));"
  , "}"
  ]

-- | Parser entry point code.
--
entryCode :: ParserMode -> CF -> String
entryCode mode cf = unlines $ map (parseMethod mode cf) eps
  where
  eps = toList (allEntryPoints cf)

--This generates a parser method for each entry point.
parseMethod :: ParserMode -> CF -> Cat -> String
parseMethod mode cf cat = unlines $ concat
  [ [ unwords [ "/* Entrypoint: parse", dat, "from file. */" ]
    , dat ++ " p" ++ parser ++ "(FILE *inp)"
    ]
  , body False
  , [ ""
    , unwords [ "/* Entrypoint: parse", dat, "from string. */" ]
    , dat ++ " ps" ++ parser ++ "(const char *str)"
    ]
  , body True
  ]
  where
  name = parserName mode
  body stringParser = concat
    [ [ "{"
      , "  YYSTYPE result;"
      , "  yyscan_t scanner = " ++ name ++ "_initialize_lexer(" ++ file ++ ");"
      , "  if (!scanner) {"
      , "    fprintf(stderr, \"Failed to initialize lexer.\\n\");"
      , "    return 0;"
      , "  }"
      ]
    , [ "  YY_BUFFER_STATE buf = " ++ name ++ "_scan_string(str, scanner);" | stringParser ]
    , [ "  int error = yyparse(scanner, &result);" ]
    , [ "  " ++ name ++ "_delete_buffer(buf, scanner);" | stringParser ]
    , [ "  " ++ name ++ "lex_destroy(scanner);"
      , "  if (error)"
      , "  { /* Failure */"
      , "    return 0;"
      , "  }"
      , "  else"
      , "  { /* Success */"
      ]
    , revOpt
    , [ "    return" +++ res ++ ";"
      , "  }"
      , "}"
      ]
    ]
    where
    file | stringParser = "0"
         | otherwise    = "inp"
  stl    = stlParser mode
  ncat   = normCat cat
  dat0   = identCat ncat
  dat    = if cParser mode then dat0 else dat0 ++ "*"
  parser = identCat cat
  res0   = concat [ "result.", varName ncat ]
  -- Reversing the result
  isReversible  = cat `elem` cfgReversibleCats cf
  -- C and NoSTL
  res
    | not stl, isReversible
                = "reverse" ++ dat0 ++ "(" ++ res0 ++ ")"
    | otherwise = res0
  -- STL: Vectors are snoc lists
  revOpt = when (stl && isList cat && not isReversible)
             [ "std::reverse(" ++ res ++ "->begin(), " ++ res ++"->end());" ]

--This method generates list reversal functions for each list type.
reverseList :: ParserMode -> Cat -> String
reverseList mode c0 = unlines
    [ c' ++ " reverse" ++ c ++ "(" ++ c' +++ "l)"
    , "{"
    , "  " ++ c' +++"prev = 0;"
    , "  " ++ c' +++"tmp = 0;"
    , "  while (l)"
    , "  {"
    , "    tmp = l->" ++ v ++ ";"
    , "    l->" ++ v +++ "= prev;"
    , "    prev = l;"
    , "    l = tmp;"
    , "  }"
    , "  return prev;"
    , "}"
    ]
  where
  c  = identCat (normCat c0)
  c' = c ++ star
  v = map toLower c ++ "_"
  star = if cParser mode then "" else "*"

-- | The union declaration is special to Bison/Yacc and gives the type of
-- yylval.  For efficiency, we may want to only include used categories here.
--
-- >>> let foo = Cat "Foo"
-- >>> union (CParser True "") [foo, ListCat foo]
-- %union
-- {
--   int    _int;
--   char   _char;
--   double _double;
--   char*  _string;
--   Foo* foo_;
--   ListFoo* listfoo_;
-- }
--
-- If the given list of categories is contains coerced categories, those should
-- be normalized and duplicate removed
-- E.g. if there is both [Foo] and [Foo2] we should only print one pointer:
--    ListFoo* listfoo_;
--
-- >>> let foo2 = CoercCat "Foo" 2
-- >>> union (CppParser Nothing "") [foo, ListCat foo, foo2, ListCat foo2]
-- %union
-- {
--   int    _int;
--   char   _char;
--   double _double;
--   char*  _string;
--   Foo* foo_;
--   ListFoo* listfoo_;
-- }
union :: ParserMode -> [Cat] -> Doc
union mode cats = vcat
    [ "%union"
    , codeblock 2 $ map text unionBuiltinTokens ++ map mkPointer normCats
    ]
  where
  normCats = nub (map normCat cats)
  mkPointer s = scope <> text (identCat s) <> star <+> text (varName s) <> ";"
  scope = text $ nsScope $ parserPackage mode
  star = if cParser mode then empty else text "*"

unionBuiltinTokens :: [String]
unionBuiltinTokens =
  [ "int    _int;"
  , "char   _char;"
  , "double _double;"
  , "char*  _string;"
  ]

-- | @%type@ declarations for non-terminal types.
declarations :: ParserMode -> CF -> String
declarations mode cf = unlines $ map typeNT $
  posCats ++
  filter (not . null . rulesForCat cf) (allParserCats cf) -- don't define internal rules
  where
  typeNT nt = "%type <" ++ varName nt ++ "> " ++ identCat nt
  posCats
    | stlParser mode = map TokenCat $ positionCats cf
    | otherwise      = []

--declares terminal types.
-- token name "literal"
-- "Syntax error messages passed to yyerror from the parser will reference the literal string instead of the token name."
-- https://www.gnu.org/software/bison/manual/html_node/Token-Decl.html
tokens :: [UserDef] -> SymMap -> [[String]]
tokens user env = map declTok $ Map.toList env
  where
  declTok (Keyword   s, r) = tok "" s r
  declTok (Tokentype s, r) = tok (if s `elem` user then "<_string>" else "") s r
  tok t s r = [ "%token" ++ t, r, " /* " ++ cStringEscape s ++ " */" ]

-- | Escape characters inside a C string.
cStringEscape :: String -> String
cStringEscape = concatMap escChar
  where
    escChar c
      | c `elem` ("\"\\" :: String) = '\\':[c]
      | otherwise = [c]

-- | Produces a table with the built-in token types.
specialToks :: CF -> [[String]]
specialToks cf = concat
  [ ifC catString  [ "%token<_string>", "_STRING_"  ]
  , ifC catChar    [ "%token<_char>  ", "_CHAR_"    ]
  , ifC catInteger [ "%token<_int>   ", "_INTEGER_" ]
  , ifC catDouble  [ "%token<_double>", "_DOUBLE_"  ]
  , ifC catIdent   [ "%token<_string>", "_IDENT_"   ]
  ]
  where
    ifC cat s = if isUsedCat cf (TokenCat cat) then [s] else []

-- | Bison only supports a single entrypoint.
startSymbol :: CF -> String
startSymbol cf = "%start" +++ identCat (firstEntry cf)

--The following functions are a (relatively) straightforward translation
--of the ones in CFtoHappy.hs
rulesForBison :: RecordPositions -> ParserMode -> CF -> SymMap -> Rules
rulesForBison rp mode cf env = map mkOne (ruleGroups cf) ++ posRules
  where
  mkOne (cat,rules) = constructRule rp mode cf env rules cat
  posRules :: Rules
  posRules
    | CppParser inPackage _ <- mode = for (positionCats cf) $ \ n -> (TokenCat n,
      [( Map.findWithDefault n (Tokentype n) env
       , addResult cf (TokenCat n) $ concat
         [ "$$ = new ", nsScope inPackage, n, "($1, @$.first_line);" ]
       )])
    | otherwise = []

-- For every non-terminal, we construct a set of rules.
constructRule
  :: RecordPositions -> ParserMode -> CF -> SymMap
  -> [Rule]                           -- ^ List of alternatives for parsing ...
  -> NonTerminal                      -- ^ ... this non-terminal.
  -> (NonTerminal,[(Pattern,Action)])
constructRule rp mode cf env rules nt = (nt,) $
    [ (p,) $ addResult cf nt $ generateAction rp mode (identCat (normCat nt)) (funRule r) b m
    | r0 <- rules
    , let (b,r) = if isConsFun (funRule r0) && valCat r0 `elem` cfgReversibleCats cf
                  then (True, revSepListRule r0)
                  else (False, r0)
    , let (p,m) = generatePatterns mode cf env r
    ]

-- | Add action if we parse an entrypoint non-terminal:
-- Set field in result record to current parse.
addResult :: CF -> NonTerminal -> Action -> Action
addResult cf nt a =
  if nt `elem` toList (allEntryPoints cf)
  -- Note: Bison has only a single entrypoint,
  -- but BNFC works around this by adding dedicated parse methods for all entrypoints.
  -- Andreas, 2021-03-24: But see #350: bison still uses only the @%start@ non-terminal.
    then concat [ a, " result->", varName (normCat nt), " = $$;" ]
    else a

-- | Switch between STL or not.
generateAction :: IsFun a
  => RecordPositions     -- ^ Remember position information?
  -> ParserMode          -- ^ For C or C++?
  -> String              -- ^ List type.
  -> a                   -- ^ Rule name.
  -> Bool                -- ^ Reverse list?
  -> [(MetaVar, Bool)]   -- ^ Meta-vars; should the list referenced by the var be reversed?
  -> Action
generateAction rp = \case
  CppParser ns _ -> generateActionSTL rp ns
  CParser   b  _ -> \ nt f r -> generateActionC rp (not b) nt f r . map fst

-- | Generates a string containing the semantic action.
-- >>> generateActionC NoRecordPositions False "Foo" "Bar" False ["$1"]
-- "$$ = new Bar($1);"
-- >>> generateActionC NoRecordPositions True "Foo" "Bar" False ["$1"]
-- "$$ = make_Bar($1);"
-- >>> generateActionC NoRecordPositions True "Foo" "_" False ["$1"]
-- "$$ = $1;"
-- >>> generateActionC NoRecordPositions True "ListFoo" "[]" False []
-- "$$ = 0;"
-- >>> generateActionC NoRecordPositions True "ListFoo" "(:[])" False ["$1"]
-- "$$ = make_ListFoo($1, 0);"
-- >>> generateActionC NoRecordPositions True "ListFoo" "(:)" False ["$1","$2"]
-- "$$ = make_ListFoo($1, $2);"
-- >>> generateActionC NoRecordPositions True "ListFoo" "(:)" True ["$1","$2"]
-- "$$ = make_ListFoo($2, $1);"
generateActionC :: IsFun a => RecordPositions -> Bool -> String -> a -> Bool -> [MetaVar] -> Action
generateActionC rp cParser nt f b ms
  | isCoercion f = "$$ = " ++ unwords ms ++ ";" ++ loc
  | isNilFun f   = "$$ = 0;"
  | isOneFun f   = concat ["$$ = ", new nt, "(", intercalate ", " ms', ", 0);"]
  | isConsFun f  = concat ["$$ = ", new nt, "(", intercalate ", " ms', ");"]
  | otherwise    = concat ["$$ = ", new (funName f), "(", intercalate ", " ms', ");", loc]
 where
  ms' = if b then reverse ms else ms
  loc | RecordPositions <- rp
          = " $$->line_number = @$.first_line; $$->char_number = @$.first_column;"
      | otherwise
          = ""
  new :: String -> String
  new | cParser   = ("make_" ++)
      | otherwise = \ s -> if isUpper (head s) then "new " ++ s else sanitizeCpp s

generateActionSTL :: IsFun a => RecordPositions -> InPackage -> String -> a -> Bool -> [(MetaVar,Bool)] -> Action
generateActionSTL rp inPackage nt f b mbs = reverses ++
  if | isCoercion f    -> concat ["$$ = ", unwords ms, ";", loc]
     | isNilFun f      -> concat ["$$ = ", "new ", scope, nt, "();"]
     | isOneFun f      -> concat ["$$ = ", "new ", scope, nt, "(); $$->push_back(", head ms, ");"]
     | isConsFun f     -> concat [lst, "->push_back(", el, "); $$ = ", lst, ";"]
     | isDefinedRule f -> concat ["$$ = ", scope, sanitizeCpp (funName f), "(", intercalate ", " ms, ");" ]
     | otherwise       -> concat ["$$ = ", "new ", scope, funName f, "(", intercalate ", " ms, ");", loc]
 where
  ms        = map fst mbs
  -- The following match only happens in the cons case:
  [el, lst] = applyWhen b reverse ms  -- b: left-recursion transformed?

  loc | RecordPositions <- rp
            = " $$->line_number = @$.first_line; $$->char_number = @$.first_column;"
      | otherwise
            = ""
  reverses  = unwords ["std::reverse(" ++ m ++"->begin(),"++m++"->end()) ;" | (m, True) <- mbs]
  scope     = nsScope inPackage

-- Generate patterns and a set of metavariables indicating
-- where in the pattern the non-terminal
generatePatterns :: ParserMode -> CF -> SymMap -> Rule -> (Pattern,[(MetaVar,Bool)])
generatePatterns mode cf env r = case rhsRule r of
  []  -> ("/* empty */",[])
  its -> (unwords (map mkIt its), metas its)
 where
   stl  = stlParser mode
   mkIt = \case
     Left (TokenCat s)
       | stl && isPositionCat cf s
                   -> typeName s
       | otherwise -> Map.findWithDefault (typeName s) (Tokentype s) env
     Left c  -> identCat c
     Right s -> Map.findWithDefault s (Keyword s) env
   metas its = [(revIf c ('$': show i), revert c) | (i, Left c) <- zip [1 :: Int ..] its]
   -- C and C++/NoSTL: call reverse function
   revIf c m = if not stl && isntCons && elem c revs
                 then "reverse" ++ identCat (normCat c) ++ "(" ++ m ++ ")"
               else m  -- no reversal in the left-recursive Cons rule itself
   -- C++/STL: flag if reversal is necessary
   -- notice: reversibility with push_back vectors is the opposite
   -- of right-recursive lists!
   revert c = isntCons && isList c && notElem c revs
   revs     = cfgReversibleCats cf
   isntCons = not $ isConsFun $ funRule r

-- We have now constructed the patterns and actions,
-- so the only thing left is to merge them into one string.

prRules :: Rules -> String
prRules [] = []
prRules ((_, []):rs) = prRules rs --internal rule
prRules ((nt, (p,a) : ls):rs) =
  unwords [nt', ":" , p, "{", a, "}", '\n' : pr ls] ++ ";\n" ++ prRules rs
 where
  nt' = identCat nt
  pr []           = []
  pr ((p,a):ls)   = unlines [unwords ["  |", p, "{", a , "}"]] ++ pr ls

--Some helper functions.
resultName :: String -> String
resultName s = "YY_RESULT_" ++ s ++ "_"

-- | slightly stronger than the NamedVariable version.
-- >>> varName (Cat "Abc")
-- "abc_"
varName :: Cat -> String
varName = \case
  TokenCat s -> "_" ++ map toLower s
  c          -> (++ "_") . map toLower . identCat . normCat $ c

typeName :: String -> String
typeName "Ident" = "_IDENT_"
typeName "String" = "_STRING_"
typeName "Char" = "_CHAR_"
typeName "Integer" = "_INTEGER_"
typeName "Double" = "_DOUBLE_"
typeName x = x
