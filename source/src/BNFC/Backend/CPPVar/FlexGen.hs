{-# LANGUAGE QuasiQuotes #-}

{-|
  Module      : BNFC.Backend.CPPVar.FlexGen
  Description : FLex grammar generator.

  FLex grammar generator.
-}

module BNFC.Backend.CPPVar.FlexGen
  (
    -- * The entrypoint
    makeFlex

    -- * Compilation output
  , CompiledLexer(..)
  , NamedImplicitTokens(..)
  , lookupImplicitTokenName

    -- * File naming
  , flexFilename

    -- * C++ scanner interface class
  , scannerDecl
  ) where

-- Language imports
import Prelude hiding ((<>))
import Data.Char (ord)
import Data.Int (Int8)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.String.QQ (s)

import Text.PrettyPrint (Doc, text, ($+$), empty, (<>))

-- BNFC imports
import qualified BNFC.CF as CF
import qualified BNFC.Options as Options
import BNFC.PrettyPrint (Pretty(..))
import BNFC.Utils (symbolToName)
import qualified BNFC.RegexMinus as Minus

import BNFC.Backend.CPPVar.CPPUtil
import qualified BNFC.Backend.CPPVar.FlexRegex as FlexRegex

-- | Returns the name of the Bison grammar file.
flexFilename ::
     Options.SharedOptions  -- ^ BNFC invokation options.
  -> String
flexFilename = (++ ".l") . Options.lang

-- | Generates the FLex grammar file.
makeFlex ::
     Options.SharedOptions  -- ^ BNFC invokation options.
  -> [CF.Literal]
  -> [String]
  -> [CF.Pragma]
  -> CompiledLexer
makeFlex opts literals terminals pragmas = CompiledLexer
  { compiledLexer_flexGrammar =
    flexHead nsutils (Options.lang opts) usedTokens
    $+$ literalTokenConditions usedTokens
    $++$ literalTokenRegexDefs usedTokens
    $++$ text "%%"
    $++$ commentBlocks pragmas
    $++$ oneLineComments pragmas
    $++$ defImplicitTokens nsutils tkNames
    $++$ defCustomTokens nsutils pragmas
    $++$ defBuiltInTokens nsutils usedTokens
    $++$ text ("<INITIAL>[\\t\\n\\f\\r\\x20]+ /* whitespace */ "
      ++ "yyextra->TakeLoc();")
    $+$ text ("<INITIAL><<EOF>> return " ++ packprefix
      ++ "Parser::make_YYEOF(yyextra->TakeLoc());")
    $++$ text ("[\\x00-\\xff] return LEXER_ERROR(yyextra->TakeLoc());")
    $++$ text "%%"
    $++$ maybeWrapNamespace (scannerDecl $++$ scannerImpl)
  , compiledLexer_implicitTokenNames = tkNames
  }
  where
    tkNames = nameAllImplicitTokens terminals
    nsutils@NamespaceUtils
      { nsutils_wrap   = maybeWrapNamespace
      , nsutils_prefix = packprefix
      }
      = newNamespaceUtilsFromOptions opts
    usedTokens = getBuiltInTokenUsage literals

------------------------------------------------------------------------
-- * Handling the generated lexer.
------------------------------------------------------------------------

-- | Returned from 'makeFlex'. Contains the FLex grammar and token naming
-- information (relevant to the syntax parser).
data CompiledLexer = CompiledLexer
  { compiledLexer_flexGrammar        :: !Doc  -- ^ The FLex grammar.
  , compiledLexer_implicitTokenNames :: !NamedImplicitTokens
    -- ^ What the terminals specified as literal strings are named.
  }

-- | What the terminals specified as literal strings are named.
-- Maps in-code token strings to their names in Bison.
-- See 'lookupImplicitTokenName'.
newtype NamedImplicitTokens = NamedImplicitTokens (Map String String)

-- | Returns the Bison identifier of a token specified in the grammar as a
-- literal string.
lookupImplicitTokenName ::
     String
    -- ^ The token how it appears in the grammar, e.g. "{"
  -> NamedImplicitTokens  -- ^ Collection.
  -> Maybe String         -- ^ The token name, e.g. @SYM_LBRACE@ for "{".
lookupImplicitTokenName name (NamedImplicitTokens mp) = name `Map.lookup` mp

------------------------------------------------------------------------
-- * Utility.
------------------------------------------------------------------------

-- | Converts colons in a C++ namespace prefix ("abc::def::") into underscores
-- ("abc__def__").
flexPrefix :: String -> String
flexPrefix = map (\ c -> if c == ':' then '_' else c)

-- | Which built-in literal tokens the grammar uses.
data BuiltInTokenUsage = BuiltInTokenUsage
  { grammarUsesIdent   :: !Bool  -- ^ Does the grammar use the @Ident@ token?
  , grammarUsesString  :: !Bool  -- ^ Does the grammar use the @String@ token?
  , grammarUsesChar    :: !Bool  -- ^ Does the grammar use the @Char@ token?
  , grammarUsesInteger :: !Bool  -- ^ Does the grammar use the @Integer@ token?
  , grammarUsesDouble  :: !Bool  -- ^ Does the grammar use the @Double@ token?
  }

-- | Converts a list of used literals into a more convenient boolean set of used
-- literals.
getBuiltInTokenUsage ::
     [CF.Literal]  -- ^ All used literals.
  -> BuiltInTokenUsage
getBuiltInTokenUsage literals = BuiltInTokenUsage
  { grammarUsesIdent   = CF.catIdent `elem` literals
  , grammarUsesString  = CF.catString `elem` literals
  , grammarUsesChar    = CF.catChar `elem` literals
  , grammarUsesInteger = CF.catInteger `elem` literals
  , grammarUsesDouble  = CF.catDouble `elem` literals
  }

-- | Assigns names to all tokens (terminals) specified in the grammar as literal
-- strings.
nameAllImplicitTokens ::
     [String]             -- ^ All used terminals (keywords and symbols).
  -> NamedImplicitTokens  -- ^ Map from the token to its name.
nameAllImplicitTokens terminals = NamedImplicitTokens $ helper 1 terminals
  where
    helper ::
         Int       -- ^ Index (for naming unrecognized symbols)
      -> [String]
      -> Map String String
    helper i ss = case ss of
      []       -> Map.empty
      s : tail -> case symbolToName s of
        Just name -> Map.insert s name $ helper i tail
        Nothing   ->
          if isCIdent s
          then Map.insert s ("KW_" ++ s)       (helper i tail)
          else Map.insert s ("SYM_" ++ show i) (helper (i + 1) tail)

-- | Returns @True@ if the name is a valid C identifier (can be used as-is).
isCIdent ::
     String  -- ^ The scrutinized name.
  -> Bool    -- ^ @True@ if it is a valid C identifier, @False@ otherwise.
isCIdent = \case
  "" -> False
  ch : tail -> alpha ch && all (\ ch -> alpha ch || digit ch) tail
    where
      alpha ch =
        ch == '_'
        || (ord 'A' <= o && o <= ord 'Z')
        || (ord 'a' <= o && o <= ord 'z')
        where o = ord ch
      digit ch = ord '0' <= o && o <= ord '9'
        where o = ord ch

------------------------------------------------------------------------
-- * Code generation (definitions section).
------------------------------------------------------------------------

-- | Generates FLex options and C++ @include@ directives.
flexHead ::
     NamespaceUtils
  -> String             -- ^ Language name.
  -> BuiltInTokenUsage  -- ^ Which literals are used in the grammar.
  -> Doc
flexHead nsutils language usedTokens = (linesToText $
  [ "%option warn nodefault"
  , "%option 8bit reentrant"
  , "%option noyywrap noinput nounput"
  ] ++ (case maybePackagePrefix of
    "" -> []
    _ -> ["%option prefix=\"" ++ flexPrefix maybePackagePrefix ++ "\""]) ++
  [ "%option outfile=\"" ++ language ++ ".lex.cpp\""
  , ""
  , "%{"
  , "#undef yylex"
  ] ++ (if useCharconv then ["#include <charconv>"] else []) ++
  [ "#include <string>"
  , "#include <string_view>"
  , "#include <system_error>"
  , "#include \"Parser.hpp\""
  ])
  $++$ packwrap (unlinesToText [s|
struct Extra {
    std::string parsed;
    position start;
    position end;

    Extra(std::string* optFilename)
        : start(optFilename, 1, 1)
        , end(optFilename, 1, 1) {}
    inline location TakeLoc() {
        location res = {start, end};
        start = end;
        return res;
    }
};

Parser::symbol_type yylex(yyscan_t yyscanner, Parser* thisparser);
|])
  $++$ text (concat
      [ "#define YY_DECL "
      , maybePackagePrefix
      , "Parser::symbol_type "
      , maybePackagePrefix
      , "yylex(yyscan_t yyscanner, Parser* thisparser)"
      ])
  $+$ unlinesToText [s|
#define YY_USER_ACTION                   \
    {                                    \
        for (int i = 0; i < yyleng; i++) \
            if (yytext[i] == '\n')       \
                yyextra->end.lines();    \
            else                         \
                yyextra->end.columns();  \
    }
|]
  $+$ linesToText
    [ concat
      [ "#define yyterminate() return "
      , maybePackagePrefix
      , "Parser::make_YYEOF(yyextra->TakeLoc())"
      ]
    , ""
    , "#define LEXER_ERROR(loc) LexerError(thisparser, loc)"
    , concat
      [ "static inline "
      , maybePackagePrefix
      , "Parser::symbol_type LexerError("
      ]
    , concat
      [ "        "
      , maybePackagePrefix
      , "Parser* thisparser, const "
      , maybePackagePrefix
      , "location& loc) {"
      ]
    , "    thisparser->error(loc, \"Lexical error\");"
    , concat
      [ "    return "
      , maybePackagePrefix
      , "Parser::make_YYerror(loc);"
      ]
    , "}"
    ]
  $++$ literalTokenUtils usedTokens $++$ linesToText
  [ "%}"
  , ""
  , "%option extra-type=\"" ++ maybePackagePrefix ++ "Extra*\""
  ]
  where
    useCharconv = grammarUsesInteger usedTokens || grammarUsesDouble usedTokens
    NamespaceUtils
      { nsutils_wrap = packwrap
      , nsutils_prefix = maybePackagePrefix
      } = nsutils

-- | Make definitions for tokens specified as literal strings.
defImplicitTokens ::
     NamespaceUtils
  -> NamedImplicitTokens    -- ^ The implicit tokens to recognize.
  -> Doc
defImplicitTokens NamespaceUtils { nsutils_prefix = packprefix }
    (NamedImplicitTokens mp) = linesToText
  [concat
    ["<INITIAL>"
    , show (pretty (FlexRegex.flexStringUTF8 str))
    , " return "
    , packprefix
    , "Parser::make_"
    , name
    , "(yyextra->TakeLoc());"
    ]
  | (str, name) <- Map.toList mp]

-- | Generates lexer start conditions (modes) used in parsing literal @String@s
-- and @Char@acters.
literalTokenConditions ::
     BuiltInTokenUsage  -- Which literals tokens the grammar needs.
  -> Doc
literalTokenConditions usedTokens =
  (if grammarUsesString usedTokens
    then text "%s STRING ESCAPE"
    else empty)
  $+$
  (if grammarUsesChar usedTokens
    then text "%s CHAR"
    else empty)

-- | Utility functions necessary for parsing the given set of literal tokens.
literalTokenUtils ::
     BuiltInTokenUsage  -- ^ Which literals tokens the grammar needs.
  -> Doc
literalTokenUtils usedTokens =
  maybeHexConversion $++$ maybeUTF8Decode $++$ maybeUTF8Encode
  where
    hasChar = grammarUsesChar usedTokens
    hasString = grammarUsesString usedTokens
    maybeHexConversion
      | hasChar || hasString = unlinesToText [s|
static inline int hexDigitValue(char ch) {
    if ('0' <= ch && ch <= '9') return ch - '0';
    if ('a' <= ch && ch <= 'f') return ch - 'a' + 10;
    if ('A' <= ch && ch <= 'F') return ch - 'A' + 10;
    return 0;
}

static inline int32_t hexInt32(const char* start, int len) {
    int32_t val = 0;
    for (int i = 0; i < len; i++) val = (val << 4) | hexDigitValue(start[i]);
    return val;
}
|]
      | otherwise = empty

    maybeUTF8Decode
      | hasChar = unlinesToText [s|
static inline int32_t minCharForEncodedLen(int len) {
    if (len < 2) return 0;
    if (len == 2) return 0x80;
    return static_cast<int32_t>(1) << (11 + 5 * (len - 3));
}

    /* returns -1 if a character uses a non-minimal # of bytes */
static inline int decodeUTF8(const char* start, int len) {
    if (len == 1) return *start;
    int firstbits = 7 - len;
    int32_t res = *start & ((1 << firstbits) - 1);
    for (int i = 1; i < len; i++) res = (res << 6) | (start[i] & 0x3f);
    return res >= minCharForEncodedLen(len) ? res : -1;
}
|]
      | otherwise = empty

    maybeUTF8Encode
      | hasString = unlinesToText [s|
static inline void encodeUTF8(std::string& dest, int32_t ch) {
    if (ch < 0) {
        dest.push_back(0xFF);
        return;
    }
    if (ch <= 0x7F) {
        dest.push_back(static_cast<char>(ch));
        return;
    }
    char buf[7];
    int sz = 0;
    while (ch) {
        buf[sz++] = ch & 0x3F;
        ch >>= 6;
    }
    buf[sz] = 0;
    sz += (static_cast<int>(buf[sz - 1]) >= 1 << (8 - sz - 1));
    buf[sz] = 0;
    for (int i = 0, j = sz - 1; i < j; ++i, --j) std::swap(buf[i], buf[j]);
    buf[0] |= static_cast<char>(0xFF << (8 - sz));
    for (int i = 1; i < sz; i++) buf[i] |= 0x80;
    dest.append(buf);
}
|]
      | otherwise = empty

-- | Generates regex definitions used within the grammar
-- (only the needed ones).
literalTokenRegexDefs ::
     BuiltInTokenUsage  -- ^ Which literals tokens the grammar needs.
  -> Doc
literalTokenRegexDefs usedTokens = maybeHexNums $++$ maybeUTF8multichar
  where
    hasChar   = grammarUsesChar   usedTokens
    hasString = grammarUsesString usedTokens
    maybeHexNums
      | hasChar || hasString = unlinesToText [s|
HEXINT [0-7][0-9a-fA-F]{7}|[0-9a-fA-F]{1,7}
HEXSHORT [0-9a-fA-F]{1,4}
HEXBYTE [0-9a-fA-F]{1,2}
|]
      | otherwise            = empty
    maybeUTF8multichar
      | hasChar   = linesToText
        [ "    /* 2-6 bytes. Does not handle the [\\x00-\\x7F] case."
        , "     * This regex permits non-minimal-length encodings,"
        , "     * but they are rejected in decodeUTF8."
        , "     */"
        , "UTF8MULTICHAR [\\xC0-\\xDF][\\x80-\\xBF]"
          ++ "|[\\xE0-\\xEF][\\x80-\\xBF]{2}|[\\xF0-\\xF7][\\x80-\\xBF]{3}"
          ++ "|[\\xF8-\\xFB][\\x80-\\xBF]{4}|[\\xFC-\\xFD][\\x80-\\xBF]{5}"
        ]
      | otherwise = empty

------------------------------------------------------------------------
-- * Code generation (rules section).
------------------------------------------------------------------------

-- | Generates rules for user-defined tokens.
defCustomTokens ::
     NamespaceUtils
  -> [CF.Pragma]  -- ^ Grammar pragmas (contain token definitions).
  -> Doc
defCustomTokens NamespaceUtils { nsutils_prefix = packprefix } pragmas =
  vcatSpaced
  [ makeCustomToken (CF.wpThing name) regex
  | CF.TokenReg name _ regex <- pragmas]
  where
    makeCustomToken name reg = text ("    /* " ++ name ++ " */")
      $+$ (text "<INITIAL>" <> pretty regFlex <> text code)
      where
        regSimple = FlexRegex.fromBNFCReg reg
        regFlex   = FlexRegex.fromSimpleRegex regSimple
        code      = concat
          [ " return "
          , packprefix
          , "Parser::make_CUSTOM_"
          , name
          , "(yytext, yyextra->TakeLoc());"
          ]

-- | Generates parsing rules for used literal tokens.
defBuiltInTokens ::
     NamespaceUtils
  -> BuiltInTokenUsage      -- ^ Which literals are used.
  -> Doc
defBuiltInTokens NamespaceUtils { nsutils_prefix = packprefix } usedTokens =
  maybeString $++$ maybeChar $++$ maybeInteger $++$ maybeDouble $++$ maybeIdent
  where
    parserName = packprefix ++ "Parser"
    maybeString
      | grammarUsesString usedTokens  = defString parserName
      | otherwise                     = empty
    maybeChar
      | grammarUsesChar usedTokens    = defChar parserName
      | otherwise                     = empty
    maybeInteger
      | grammarUsesInteger usedTokens = defInteger parserName
      | otherwise                     = empty
    maybeDouble
      | grammarUsesDouble usedTokens  = defDouble parserName
      | otherwise                     = empty
    maybeIdent
      | grammarUsesIdent usedTokens   = defIdent parserName
      | otherwise                     = empty

-- | @Ident@ rule.
defIdent ::
     String  -- ^ Bison parser name.
  -> Doc
defIdent parser = text $ "<INITIAL>[a-zA-Z_][a-zA-Z0-9_]* return "
    ++ parser ++ "::make_IDENT(yytext, yyextra->TakeLoc());"

-- | @String@ rules.
defString ::
     String  -- ^ Bison parser name.
  -> Doc
defString parser = linesToText
    [ "    /* String */"
    , "<INITIAL>\\\" BEGIN(STRING); yyextra->parsed.clear();"
    , "<STRING>\\\" {"
    , "        BEGIN(INITIAL);"
    , "        std::string result;"
    , "        result.swap(yyextra->parsed);"
    , "        return " ++ parser ++ "::make_STRING(result, yyextra->TakeLoc());"
    , "    }"
    ] $+$ [s|
<STRING>\\ BEGIN(ESCAPE);
<STRING>. yyextra->parsed.push_back(*yytext);
<ESCAPE>0 BEGIN(STRING); yyextra->parsed.push_back('\0');
<ESCAPE>a BEGIN(STRING); yyextra->parsed.push_back('\a');
<ESCAPE>b BEGIN(STRING); yyextra->parsed.push_back('\b');
<ESCAPE>f BEGIN(STRING); yyextra->parsed.push_back('\f');
<ESCAPE>n BEGIN(STRING); yyextra->parsed.push_back('\n');
<ESCAPE>r BEGIN(STRING); yyextra->parsed.push_back('\r');
<ESCAPE>t BEGIN(STRING); yyextra->parsed.push_back('\t');
<ESCAPE>v BEGIN(STRING); yyextra->parsed.push_back('\v');
<ESCAPE>x{HEXBYTE} {
        BEGIN(STRING);
        yyextra->parsed.push_back(
            static_cast<char>(hexInt32(yytext + 1, yyleng - 1)));
    }
<ESCAPE>u{HEXSHORT} |
<ESCAPE>U{HEXINT} {
        BEGIN(STRING);
        encodeUTF8(yyextra->parsed, hexInt32(yytext + 1, yyleng - 1));
    }
<ESCAPE>. BEGIN(STRING); yyextra->parsed.push_back(*yytext);
|]

-- | @Double@ rules.
defDouble ::
     String  -- ^ Bison parser name.
  -> Doc
defDouble parser = unlinesToText [s|
    /* Double */
<INITIAL>[0-9]+(\.[0-9]+)?([eE][+\-]?[0-9]+)? {
        const char* const end = yytext + yyleng;
        double num;
        auto res = std::from_chars(yytext, end, num);
        auto loc = yyextra->TakeLoc();
        if (res.ec == std::errc() && res.ptr == end)
|] $+$ linesToText
    [ "            return " ++ parser ++ "::make_DOUBLE(num, loc);"
    , "        return LEXER_ERROR(loc);"
    , "    }"
    ]

-- | @Integer@ rules.
defInteger ::
     String  -- ^ Bison parser name.
  -> Doc
defInteger parser = unlinesToText [s|
    /* Integer (must be above Double) */
<INITIAL>[0-9]+ {
        const char* const end = yytext + yyleng;
        long num;
        auto res = std::from_chars(yytext, end, num);
        auto loc = yyextra->TakeLoc();
        if (res.ec == std::errc() && res.ptr == end)
|] $+$ linesToText
    [ "            return " ++ parser ++ "::make_INTEGER(num, loc);"
    , "        return LEXER_ERROR(loc);"
    , "    }"
    ]

-- | @Char@ rules.
defChar ::
     String  -- ^ Bison parser name.
  -> Doc
defChar parser = linesToText (
    [ "    /* Char in UTF-8 */"
    , "<INITIAL>' BEGIN(CHAR);"
    ] ++ map simpleEscape "0abfnrtv" ++
    [ "<CHAR>\\x{HEXBYTE}'  |"
    , "<CHAR>\\u{HEXSHORT}' |"
    , "<CHAR>\\U{HEXINT}' {"
    , "        BEGIN(INITIAL);"
    , "        return " ++ parser
      ++ "::make_CHAR(hexInt32(yytext + 2, yyleng - 2), yyextra->TakeLoc());"
    , "    }"
    , "<CHAR>\\\\({UTF8MULTICHAR}|.)' {"
    , "        BEGIN(INITIAL);"
    , "        int32_t charcode = decodeUTF8(yytext + 1, yyleng - 2);"
    , "        auto loc = yyextra->TakeLoc();"
    , "        if (charcode == -1) return LEXER_ERROR(loc);"
    , "        return " ++ parser ++ "::make_CHAR(charcode, loc);"
    , "    }"
    , "<CHAR>({UTF8MULTICHAR}|[^'\\\\\\n])' {"
    , "        BEGIN(INITIAL);"
    , "        int32_t charcode = decodeUTF8(yytext, yyleng - 1);"
    , "        auto loc = yyextra->TakeLoc();"
    , "        if (charcode == -1) return LEXER_ERROR(loc);"
    , "        return " ++ parser ++ "::make_CHAR(charcode, loc);"
    , "    }"
    ])
  where
    simpleEscape ch = concat
      [ "<CHAR>\\\\"
      , [ch]
      , "' BEGIN(INITIAL); return "
      , parser
      , "::make_CHAR('\\"
      , [ch]
      , "', yyextra->TakeLoc());"
      ]

-- | Rules to discard one-line comments.
oneLineComments ::
     [CF.Pragma]  -- ^ Grammar pragmas (contain one-line comment definitions).
  -> Doc
oneLineComments pragmas = foldr ($+$) empty
  [ text "<INITIAL>" <> pretty (FlexRegex.flexStringUTF8 s) <> text ".* ;"
  | CF.CommentS s <- pragmas]

-- | Rules to discard block comments.
commentBlocks ::
     [CF.Pragma]  -- ^ Grammar pragmas (contain block comment definitions).
  -> Doc
commentBlocks pragmas = foldr ($++$) empty
  [makeRule start end | CF.CommentM (start, end) <- pragmas]
  where
    makeRule start end = let
        -- {start}
        flexStart  = FlexRegex.flexStringUTF8 start
        -- {end}
        simpleEnd  = FlexRegex.simpleStringUTF8 end
        -- {any} := [\0-\xff]
        anyByte    = [-128..127] :: [Int8]
        -- {any}*
        simpleAny  = Minus.Rep $ Minus.charset (anyByte)
        -- {any}*{end}{any}*
        withEnd    = simpleAny `Minus.Seq` simpleEnd `Minus.Seq` simpleAny
        -- {any}*-{any}*{end}{any}*
        withoutEnd = FlexRegex.fromSimpleRegex $ simpleAny `Minus.Sub` withEnd
        -- {start}({any}*-{any}*{end}{any}*){end}
        block      = flexStart `FlexRegex.Concat` withoutEnd
          `FlexRegex.Concat` FlexRegex.fromSimpleRegex simpleEnd
      in text "<INITIAL>" <> pretty block <> text " ;"

-- | Generates the scanner class declaration. The @Scanner@ class is the
-- interface to the FLex lexer. Exported so that the Bison grammar could declare
-- the same class and link with it.
scannerDecl :: Doc
scannerDecl = unlinesToText [s|
// optFilename is not owned by the lexer.
class FlexScanner {
    yyscan_t scanner;
    FlexScanner(std::string* optFilename);

public:
    FlexScanner(FILE* file, std::string* optFilename);
    FlexScanner(std::string_view str, std::string* optFilename);
    yyscan_t Get() const;
    ~FlexScanner();
};
|]

-- | Implementations for Scanner methods (see 'scannerDecl').
scannerImpl :: Doc
scannerImpl = unlinesToText [s|
FlexScanner::FlexScanner(std::string* optFilename) {
    int err = yylex_init_extra(new Extra(optFilename), &scanner);
    if (err) throw std::system_error(err, std::generic_category(),
        "Cannot create scanner");
}

FlexScanner::FlexScanner(FILE* file, std::string* optFilename)
    : FlexScanner(optFilename) {
    yyrestart(file, scanner);
}

FlexScanner::FlexScanner(std::string_view str, std::string* optFilename)
    : FlexScanner(optFilename) {
    yy_scan_bytes(str.data(), str.size(), scanner);
}

yyscan_t FlexScanner::Get() const { return scanner; }

FlexScanner::~FlexScanner() {
    delete yyget_extra(scanner);
    yylex_destroy(scanner);
}
|]
