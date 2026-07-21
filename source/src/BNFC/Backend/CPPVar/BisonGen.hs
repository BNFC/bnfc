{-# LANGUAGE QuasiQuotes #-}
{-|
  Module      : BNFC.Backend.CPPVar.BisonGen
  Description : Bison grammar generator.

  Bison grammar generator.
-}

module BNFC.Backend.CPPVar.BisonGen
  (
    -- * The entrypoints
    makeBison
  , makeLocationHeader

    -- * File naming
  , bisonFilename
  , locationHeaderFilename
  ) where

-- Language imports
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.String.QQ (s)

import Text.PrettyPrint

-- BNFC imports
import qualified BNFC.Options as Options
import qualified BNFC.CF as CF
import BNFC.CF (CF)

import BNFC.Backend.CPPVar.CPPUtil
import qualified BNFC.Backend.CPPVar.FlexGen as FlexGen
import qualified BNFC.Backend.CPPVar.AbsynGen as AbsynGen

-- | Returns the name of the Bison grammar file.
bisonFilename ::
     Options.SharedOptions  -- ^ BNFC invokation options.
  -> String
bisonFilename opts = Options.lang opts ++ ".ypp"

locationHeaderFilename :: String
locationHeaderFilename = "Locations.hpp"

-- | Generates the Bison grammar file.
makeBison ::
     Options.SharedOptions  -- ^ BNFC invokation options.
  -> FlexGen.NamedImplicitTokens
    -- ^ What the terminals specified as literal strings are named.
  -> (String -> Bool)  -- ^ Checks if a token tracks its location.
  -> CF  -- ^ Grammar description.
  -> GroupedRules
    -- ^ Rules grouped by the category with precedence level.
  -> AbsynGen.ListItemStorage
    -- ^ How to access list elements.
  -> [CF.Cat]  -- ^ Entrypoints.
  -> Doc
makeBison opts implicitTokenNames isPosToken CF.CFG
    { cfgLiterals = literals
    , cfgPragmas = pragmas
    , cfgReversibleCats = reversibleCatList
    }
    groupedRules@(GroupedRules rulemap) storeListItemsBy
    entrypoints =
  bisonHeader utils (Options.lang opts)
  $++$ tokenDefs implicitTokenNames literals pragmas
  $++$ codeRequires utils entrypoints
  $++$ nonterms groupedRules
  $++$ codeProvides utils
  $++$ codeLex utils
  $++$ text "%start __start__"
  $++$ text "%%"
  $++$ startRules utils entrypoints
  $++$ vcatSpaced (map
      (uncurry $ category utils implicitTokenNames storeListItemsBy reversible)
      $ Map.toList rulemap)
  $++$ text "%%"
  $++$ codeSection utils
  where
    utils = newBisonUtils opts isPosToken
    reversible = Set.fromList reversibleCatList

-- | Contents of the locations header file.
makeLocationHeader :: Options.SharedOptions -> Doc
makeLocationHeader opts = linesToText
  [ "#pragma once"
  , "#include \"" ++ Options.lang opts ++ ".loc.hpp\""
  ]
  $+$ globalImports
  where
    NamespaceUtils
      { nsutils_name = packname
      } = newNamespaceUtilsFromOptions opts
    globalImports
      | null packname = unlinesToText [s|
using yy::location;
using yy::position;
|]
      | otherwise = unlinesToText [s|
// Yes, this file simply includes another file.
// It does other things if BNFC is invoked without -p.
|]

------------------------------------------------------------------------
-- * General utility.
------------------------------------------------------------------------

-- | A collection of commonly used functions that all depend on the options.
data BisonUtils = BisonUtils
  {
    -- | Has the package name been specified?
    bison_inPackage  :: Bool
    -- | Namespace handling.
  , bison_nsutils    :: NamespaceUtils
    -- | Tells if a token tracks its location.
  , bison_isPosToken :: String -> Bool
    -- | If needed, prepends the correct current location to the argument list.
  , bisonLoc_maybePrependConstructorArg ::
         [String]  -- The right-hand side arguments.
      -> [String]
    -- | Empty OR reassignment of the current location (used for lists).
    -- If not empty, the string starts with no spaces and ends in a "; ".
  , bisonLoc_maybeSet :: String
    -- | Makes an rvalue from a right-hand-side category. Takes positional
    -- tokens into account.
  , bisonLoc_makeConstructorArg ::
         CF.Cat  -- The object to move from.
      -> Int     -- The object right-hand-side index ("$n").
      -> String
  , bisonLoc_tokenConstructorArgs ::
         String  -- The token name.
      -> Int     -- The object right-hand-side index ("$n").
      -> [String]
  }

-- | Constructs a t'BisonUtils' record respecting the
-- 'BNFC.Options.inPackage' value.
newBisonUtils ::
     Options.SharedOptions  -- ^ BNFC invokation options.
  -> (String -> Bool)       -- ^ Checks if a token is positional.
  -> BisonUtils
newBisonUtils opts isPosToken = case locKind of
  CppLocationsNone -> initial
    { bisonLoc_maybePrependConstructorArg = id
    , bisonLoc_maybeSet = ""
    , bisonLoc_makeConstructorArg = const stdMoveFrom
    , bisonLoc_tokenConstructorArgs = const (( : []) . stdMoveFrom)
    }
  CppLocationsStart -> initial
    { bisonLoc_maybePrependConstructorArg = ("@$.begin" : )
    , bisonLoc_maybeSet = "$$.loc = @$.begin; "
    , bisonLoc_makeConstructorArg = \case
        CF.TokenCat name -> \ i -> concat
          [ name
          , "("
          , intercalate ", " $ tokenCtorArgs True name i
          , ")"
          ]
        _ -> stdMoveFrom
    , bisonLoc_tokenConstructorArgs = tokenCtorArgs True
    }
  CppLocationsRange -> initial
    { bisonLoc_maybePrependConstructorArg = ("@$" : )
    , bisonLoc_maybeSet = "$$.loc = @$; "
    , bisonLoc_makeConstructorArg = \case
        CF.TokenCat name -> \ i -> concat
          [ name
          , "("
          , intercalate ", " $ tokenCtorArgs False name i
          , ")"
          ]
        _ -> stdMoveFrom
    , bisonLoc_tokenConstructorArgs = tokenCtorArgs False
    }
  where
    nsutils = newNamespaceUtilsFromOptions opts
    initial = BisonUtils
      { bison_inPackage  = not $ null $ nsutils_name nsutils
      , bison_isPosToken = isPosToken
      , bison_nsutils    = nsutils
      , bisonLoc_makeConstructorArg         = undefined
      , bisonLoc_maybeSet                   = undefined
      , bisonLoc_maybePrependConstructorArg = undefined
      , bisonLoc_tokenConstructorArgs       = undefined
      }
    locKind = getLocationKind opts
    tokenCtorArgs isStart name i =
      (if isPosToken name
        then (concat ["@", show i, if isStart then ".begin" else ""] : )
        else id)
      [stdMoveFrom i]
    stdMoveFrom :: Int -> String
    stdMoveFrom i = "std::move($" ++ show i ++ ")"

-- | Information about a literal token
-- (String, Ident, Integer, Double, or Char).
data LiteralTokenInfo = LiteralTokenInfo
  { literalTokenName        :: String
    -- ^ How the token is named in the Bison grammar (i.e. the upper-case name).
  , literalTokenStorageType :: String
    -- ^ The C++ type of the value that the token holds
    -- (e.g. @std::string@ for the String token).
  }

-- | Maps the token category name to its properties relevant to Bison.
literalTokenInfo :: Map String LiteralTokenInfo
literalTokenInfo = Map.fromList
  [ (CF.catIdent, LiteralTokenInfo
      { literalTokenName        = "IDENT"
      , literalTokenStorageType = "std::string"
      })
  , (CF.catChar, LiteralTokenInfo
      { literalTokenName        = "CHAR"
      , literalTokenStorageType = "int32_t"
      })
  , (CF.catInteger, LiteralTokenInfo
      { literalTokenName        = "INTEGER"
      , literalTokenStorageType = "long"
      })
  , (CF.catString, LiteralTokenInfo
      { literalTokenName        = "STRING"
      , literalTokenStorageType = "std::string"
      })
  , (CF.catDouble, LiteralTokenInfo
      { literalTokenName        = "DOUBLE"
      , literalTokenStorageType = "double"
      })
  ]

-- | Finds the token information by its name. Throws if given an unknown name.
lookupLiteralTokenInfo :: String -> LiteralTokenInfo
lookupLiteralTokenInfo catname =
  case catname `Map.lookup` literalTokenInfo of
    Nothing  -> error $ "Unsupported literal token: " ++ catname
    Just res -> res

-- | Wraps a 'Doc' in curly braces, indenting it by 4 spaces.
-- Puts an arbitrary string as the header.
bisonBraces ::
     String  -- ^ The header.
  -> Doc     -- ^ Block to wrap and indent.
  -> Doc
bisonBraces s d = text (s ++ " {") $+$ nest 4 d $+$ text "}"

------------------------------------------------------------------------
-- * Bison top section.
------------------------------------------------------------------------

-- | Generates directives at the top of the file.
bisonHeader ::
     BisonUtils
  -> String  -- ^ Language name.
  -> Doc
bisonHeader utils@BisonUtils { bison_nsutils = nsutils } langname =
  linesToText
    [ "%require \"3.2\""
    , "%language \"c++\""
    , "%define api.value.type variant"
    , "%define api.token.constructor"
    , "%define api.parser.class {Parser}"
    ]
  $+$ (
    if bison_inPackage utils
    then text ("%define api.namespace {" ++ nsutils_name nsutils ++ "}")
    else empty
  ) $+$ linesToText
    [ "%locations"
    , "%define api.location.file \"" ++ langname ++ ".loc.hpp\""
    , "%param {yyscan_t scanner}"
    , "%param {Parser* thisparser}"
    , "%parse-param {std::variant<ParseResultVariant, "
      ++ "syntax_error>* result}"
    , "%header \"Parser.hpp\""
    ]

-- | Generates the token definitions.
tokenDefs ::
     FlexGen.NamedImplicitTokens
    -- ^ What the terminals specified as literal strings are named.
  -> [CF.Literal]  -- ^ All used built-in tokens.
  -> [CF.Pragma]   -- ^ Grammar pragmas (contain user-defined tokens).
  -> Doc
tokenDefs (FlexGen.NamedImplicitTokens implicit) literals pragmas = linesToText
  ["%token " ++ tkname | tkname <- Map.elems implicit]
  $+$ linesToText (map lit2token literals)
  $+$ linesToText (map (tokenDef "std::string")
    ["CUSTOM_" ++ CF.wpThing name
    | CF.TokenReg name _ _ <- pragmas])
  where
    tokenDef storageType name = concat ["%token <", storageType, "> " , name]
    lit2token catname =
      let info = lookupLiteralTokenInfo catname
      in tokenDef (literalTokenStorageType info) (literalTokenName info)

-- | Generates code that includes necessary definitions for the parser.
codeRequires ::
     BisonUtils
  -> [CF.Cat]  -- ^ Grammar entrypoints.
  -> Doc
codeRequires BisonUtils
    { bison_nsutils = NamespaceUtils
      { nsutils_wrap   = packwrap
      , nsutils_prefix = nsprefix
      }
    } entrypts =
  bisonBraces "%code requires" $ linesToText
  [ "#include <string_view>"
  , "#include <variant>"
  , "#include \"Absyn.hpp\""
  ] $++$ text "using yyscan_t = void*;"
  $++$ packwrap
    (text ("using ParseResultVariant = std::variant<"
      ++ intercalate ", "
        [ nsprefix ++ catNameNoCoerc cat
        | cat <- removePrecedenceFromCats entrypts  -- deduplicates
        ]
      ++ ">;"))

-- | Generates declarations of nonterminals (BNFC categories).
nonterms :: GroupedRules -> Doc
nonterms (GroupedRules rulemap) = linesToText $ map nonterm $ Map.keys rulemap
  where
    nonterm cat = concat
      [ "%nterm <"
      , nontokenCatNameNoCoerc cat
      , "> "
      , nontokenCatNameWithCoerc cat
      ]

-- | Generates the public declarations used by the client.
codeProvides ::
     BisonUtils
  -> Doc
codeProvides utils = bisonBraces "%code provides"
  $ nsutils_wrap (bison_nsutils utils)
    $ maybeImportClasses $+$ unlinesToText [s|
using ParseResultOrError =
    std::variant<ParseResultVariant, Parser::syntax_error>;
ParseResultOrError Parse(FILE* file, std::string* optFilename = nullptr);
ParseResultOrError Parse(std::string_view str,
                         std::string* optFilename = nullptr);
std::string_view ParsedNodeName(const ParseResultVariant&);

template <class T>
std::variant<T, Parser::syntax_error>
EnsureParsedType(ParseResultOrError&& parsed,
                 std::string* optFilename = nullptr) {
    if (auto* err = std::get_if<Parser::syntax_error>(&parsed))
        return std::move(*err);
    auto& var = std::get<ParseResultVariant>(parsed);
    if (auto* target = std::get_if<T>(&var))
        return std::move(*target);
    std::string msg = "Unexpected syntax: tried to parse ";
    msg
        .append(reflection::SyntaxNodeName<T>)
        .append(", but got ")
        .append(ParsedNodeName(var));
    return Parser::syntax_error(
      location(position{optFilename, 1, 1}, position{optFilename, 1, 1}),
      msg);
}

template <class T>
std::variant<T, Parser::syntax_error> ParseAs(FILE* file,
        std::string* optFilename = nullptr) {
    static_assert(reflection::IsParserEntrypoint<T>,
        "Cannot parse this class");
    return EnsureParsedType<T>(Parse(file, optFilename), optFilename);
}
template <class T>
std::variant<T, Parser::syntax_error> ParseAs(std::string_view str,
        std::string* optFilename = nullptr) {
    static_assert(reflection::IsParserEntrypoint<T>,
        "Cannot parse this class");
    return EnsureParsedType<T>(Parse(str, optFilename), optFilename);
}
|]
  where
    -- | Bison generates the parser class and location structs in the
    -- `yy` namespace when no custom package name is provided. If we want the
    -- BNFC-generated parser to be contained in the global namespace, we have
    -- to import the classes explicitly.
    maybeImportClasses
      | bison_inPackage utils = empty
      | otherwise             = text "using yy::Parser;"
      -- location classes are imported in Locations.hpp, included in Absyn.hpp

-- | Generates a bit of code that makes the lexer available.
codeLex :: BisonUtils -> Doc
codeLex utils = bisonBraces "%code"
  $ nsutils_wrap (bison_nsutils utils)
  $ text "extern Parser::symbol_type yylex(yyscan_t scanner, Parser* parser);"

------------------------------------------------------------------------
-- * Utility for the Bison rule section.
------------------------------------------------------------------------

-- | Formats the alternative bison rules.
bisonRules ::
     [String]  -- ^ A list of rules.
  -> Doc
bisonRules ls = nest 4
  $ (case ls of
      []           -> empty
      first : tail -> text (": " ++ first) $+$ linesToText (map ("| " ++) tail)
    ) $+$ text ";"

-- | Converts a single BNFC sentence form nonterminal to its Bison rule name.
sentFormCatToBisonName :: CF.Cat -> String
sentFormCatToBisonName = \case
  CF.TokenCat tokenName ->
    case Map.lookup tokenName literalTokenInfo of
      Nothing   -> "CUSTOM_" ++ tokenName
      Just info -> literalTokenName info
  cat                   -> catNameWithCoerc cat

------------------------------------------------------------------------
-- * Bison rule section.
------------------------------------------------------------------------

-- | Generates the entrypoint alternatives as grammar rules.
startRules ::
     BisonUtils
  -> [CF.Cat]  -- ^ Grammar entrypoints.
  -> Doc
startRules utils entrypoints =
  text "__start__" $+$ bisonRules (map rule entrypoints)
  where
    rule cat =
      let classname = catNameNoCoerc cat
      in concat
        [ sentFormCatToBisonName cat
        , " YYEOF { result->emplace<ParseResultVariant>("
        , "std::in_place_type_t<"
        , classname
        , ">()"
        , concat
          [ ',' : ' ' : arg
          | arg <- bisonLoc_tokenConstructorArgs utils classname 1
          ]
        , "); }"
        ]

-- | Generates all Bison rules for a category.
category ::
     BisonUtils
  -> FlexGen.NamedImplicitTokens
    -- ^ What the terminals specified as literal strings are named.
  -> AbsynGen.ListItemStorage
  -> Set CF.Cat           -- ^ The set of reversible categories.
  -> NontokenCategory  -- ^ The nonterminal.
  -> [CF.Rule]         -- ^ The rules that produce the nonterminal.
  -> Doc
category utils@BisonUtils
    { bisonLoc_maybePrependConstructorArg = prependLocArg
    , bisonLoc_makeConstructorArg = makeArg
    } (FlexGen.NamedImplicitTokens implicitTokenNames)
    storeListItemsBy reversibleCats cat rules =
  case cat of
    Nontoken_ListCat lElem -> makeCategoryFromRules
      [makeRule r | r <- rules, CF.internal r == CF.Parsable]
      where
        BisonUtils
          { bisonLoc_maybeSet = maybeUpdateLoc
          } = utils
        lElemClass = catNameNoCoerc lElem
        myMaybeMakeUnique = maybeMakeUnique lElemClass
        revConsRule' = revConsRule maybeUpdateLoc
        consRule' = consRule maybeUpdateLoc
        makeRule r =
          let rhs = CF.rhsRule $ CF.removeWhiteSpaceSeparators r
          in case CF.funName r of
            "_"     -> coercionRule rhs
            "(:)"   ->
              if CF.ListCat lElem `Set.member` reversibleCats
              then revConsRule' lElemClass rhs
              else consRule' lElemClass rhs
            "(:[])" -> concat
              [ "/* (:[]) */ "
              , sentFormToBison rhs
              , " { $$.push_front("
              , myMaybeMakeUnique $ concat
                [ "std::move($"
                , show dollarItem
                , ")"
                ]
              , "); }"
              ]
              where
                [dollarItem] = rhsObjectIndices rhs
            "[]"    -> concat
              [ "/* [] */ "
              , sentFormToBison rhs
              , " { "
              , maybeUpdateLoc
              , "}"
              ]
            name    -> error ("Invalid name for a list category: " ++ name)

    -- non-list
    _ -> makeCategoryFromRules
      [makeRule r | r <- rules, CF.internal r == CF.Parsable]
      where
        makeRule r = case CF.funName r of
          "_"  -> coercionRule (CF.rhsRule r)
          name ->
            if isClassLabel name
            then emplacementRule name (CF.rhsRule r)
            else functionRule    name (CF.rhsRule r)
  where
    -- | Does not create a category if given an empty list of rules.
    makeCategoryFromRules :: [String] -> Doc
    makeCategoryFromRules = \case
      [] -> empty
      nonempty -> text (nontokenCatNameWithCoerc cat) $+$ bisonRules nonempty
    coercionRule :: CF.SentForm -> String
    coercionRule rhs = concat
        [ "/* _ */ "
        , sentFormToBison rhs
        , " { $$ = std::move($"
        , show $ case rhsObjectIndices rhs of
          [i] -> i
          _   -> error "Coercion object count /= 1"
        , "); }"
        ]
    consRule :: String -> String -> CF.SentForm -> String
    consRule maybeLocUpdate lElemClass rhs =
      let [dollarItem, dollarList] = rhsObjectIndices rhs
      in concat
        [ "/* (:) */ "
        , sentFormToBison rhs
        , " { $$ = std::move($"
        , show dollarList
        , "); $$.push_front("
        , maybeMakeUnique lElemClass $ concat
          [ "std::move($"
          , show dollarItem
          , ")"
          ]
        , "); "
        , maybeLocUpdate
        , "}"
        ]
    -- | If rhs == [Left item, Right terminator..., Left lst],
    -- then rhs := [Left lst, Left item, Right terminator...]
    revConsRule :: String -> String -> CF.SentForm -> String
    revConsRule maybeLocUpdate lElemClass rhs =
      let (last, notlast) = myForceUnsnoc rhs
      in concat
        [ "/* flip (:) */ "
        , sentFormToBison $ last : notlast
        , " { $$ = std::move($1); $$.push_back("
        , maybeMakeUnique lElemClass "std::move($2)"
        , "); "
        , maybeLocUpdate
        , "}"
        ]
    maybeMakeUnique elemClass = case storeListItemsBy of
      AbsynGen.StoreByValue   -> id
      AbsynGen.StoreByPointer -> \ s -> concat
        ["std::make_unique<", elemClass, ">(", s, ")"]
    myForceUnsnoc = \case
      [] -> error "myForceUnsnoc called on an empty list"
      [single] -> (single, [])
      head : tail ->
        let (last, ttail) = myForceUnsnoc tail
        in (last, head : ttail)
    emplacementRule name rhs = concat
      [ "/* "
      , name
      , " */ "
      , sentFormToBison rhs
      , " { $$.emplace<"
      , name
      , ">("
      , makeArgs rhs
      , "); }"
      ]
    functionRule name rhs = concat
      [ "/* "
      , name
      , " */ "
      , sentFormToBison rhs
      , " { $$ = make_"
      , name
      , "("
      , makeArgs rhs
      , "); }"
      ]
    sentFormToBison :: CF.SentForm -> String
    sentFormToBison = unwords . unempty . map (\case
      Left cat -> sentFormCatToBisonName cat
      Right s  -> case s `Map.lookup` implicitTokenNames of
          Nothing   -> error "string token not named"
          Just name -> name)
      where
        unempty = \case
          []       -> ["/* empty */"]
          nonempty -> nonempty
    rhsObjectIndicesWithCats :: CF.SentForm -> [(Int, CF.Cat)]
    rhsObjectIndicesWithCats rhs = [(i, cat) | (Left cat, i) <- zip rhs [1..]]
    rhsObjectIndices :: CF.SentForm -> [Int]
    rhsObjectIndices = map fst . rhsObjectIndicesWithCats
    makeArgs :: CF.SentForm -> String
    makeArgs rhs = intercalate ", " $ prependLocArg
      [ makeArg cat i
      | (i, cat) <- rhsObjectIndicesWithCats rhs
      ]

------------------------------------------------------------------------
-- * Implementations.
------------------------------------------------------------------------

-- | Generates the implementations for the parsing methods.
codeSection ::
     BisonUtils
  -> Doc
codeSection BisonUtils
    { bison_nsutils = NamespaceUtils
      { nsutils_wrap = packwrap
      }
    } = packwrap $
  FlexGen.scannerDecl
  $++$ unlinesToText [s|
void Parser::error(const location& loc, const std::string& msg) {
      result->emplace<Parser::syntax_error>(loc, msg);
}

static ParseResultOrError Parse(const FlexScanner& scanner,
                                std::string* optFilename) {
    ParseResultOrError res(std::in_place_type_t<Parser::syntax_error>(),
        location(position(optFilename, 1, 1), position(optFilename, 1, 1)),
        "Unknown parser error");
    Parser parser(scanner.Get(), &parser, &res);
    parser.parse();
    return res;
}

ParseResultOrError Parse(FILE* file, std::string* optFilename) {
    return Parse(FlexScanner(file, optFilename), optFilename);
}

ParseResultOrError Parse(std::string_view str, std::string* optFilename) {
    return Parse(FlexScanner(str, optFilename), optFilename);
}

std::string_view ParsedNodeName(const ParseResultVariant& var) {
    return std::visit([](const auto& node) -> std::string_view {
        return reflection::SyntaxNodeName<std::decay_t<decltype(node)>>;
    }, var);
}
|]
