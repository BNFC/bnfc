{-# LANGUAGE QuasiQuotes #-}

{-|
  Module      : BNFC.Backend.CPPVar.ClassicPrettyPrinterGen
  Description : Stateful token-based pretty-printing
                of the abstract syntax tree.

  Stateful token-based pretty-printing of the abstract syntax tree.
-}

module BNFC.Backend.CPPVar.ClassicPrettyPrinterGen
  (
    -- * The entrypoint
    makeClassicPrettyPrinter

    -- * File naming
  , classicPrettyPrinterHppFilename
  , classicPrettyPrinterCppFilename
  ) where

-- Language imports
import Data.String.QQ

import Text.PrettyPrint hiding (Str)

-- BNFC imports
import qualified BNFC.Options
import qualified BNFC.CF as CF

import BNFC.Backend.CPPVar.CPPUtil
import BNFC.Backend.CPPVar.PrinterUtils
import BNFC.Backend.CPPVar.AbsynGen (tokenStorageName, ListItemStorage(..))

------------------------------------------------------------------------
-- * File naming.
------------------------------------------------------------------------

-- | The name of the header file.
classicPrettyPrinterHppFilename :: String
classicPrettyPrinterHppFilename = prettyPrinterClassName ++ ".hpp"

-- | The name of the source file.
classicPrettyPrinterCppFilename :: String
classicPrettyPrinterCppFilename = prettyPrinterClassName ++ ".cpp"

------------------------------------------------------------------------
-- * The entrypoint.
------------------------------------------------------------------------

prettyPrinterClassName :: String
prettyPrinterClassName = "ClassicPrettyPrinter"

-- | Generates the @ClassicPrettyPrinter@ class
-- (declaration and implementation).
makeClassicPrettyPrinter ::
     BNFC.Options.SharedOptions  -- ^ BNFC invokation options.
  -> [PrintableSymbol]           -- ^ The list of types to make methods for.
  -> ListItemStorage             -- ^ How to access list elements.
  -> CPPHeaderSourcePair
makeClassicPrettyPrinter opts printable listItemStorage = CPPHeaderSourcePair
  { cppHeaderText = hpp
  , cppSourceText = cpp
  }
  where
    packwrap = nsutils_wrap $ newNamespaceUtilsFromOptions opts

    hpp = makePrinterHeaderFile prettyPrinterClassName (unlinesToText [s|
// See ClassicPrettyPrinter.cpp for a description of the printing rules.

#pragma once
#include <iostream>
#include <string_view>
#include "Absyn.hpp"
|])
      (unlinesToText [s|
    std::ostream& out;
    unsigned int tabSize;
    unsigned int indentInTabs = 0;
    int coercionLevel = 0;
    bool needSpace = false;
    bool needNewline = false;
    ClassicPrettyPrinter& FlushHere();
    ClassicPrettyPrinter& SimplePut(std::string_view);

public:
    ClassicPrettyPrinter(std::ostream&, unsigned int tabSize = 4);
    unsigned int TabSize() const;
    unsigned int IndentationLevel() const;  // In tabs
    int CoercionLevel() const;

    ClassicPrettyPrinter& WithTabSize(unsigned int tabSize);
    ClassicPrettyPrinter& WithCoercionLevel(int lvl);
    ClassicPrettyPrinter& Indent(unsigned int tabs = 1);
    ClassicPrettyPrinter& Dedent(unsigned int tabs = 1);
    ClassicPrettyPrinter& WithIndent(unsigned int tabs);
    ClassicPrettyPrinter& SkipSpaceHere();
    ClassicPrettyPrinter& NeedSpaceHere();

    ClassicPrettyPrinter& PutToken(std::string_view);
    ClassicPrettyPrinter& PutCharLiteral(int32_t);
    ClassicPrettyPrinter& PutStringLiteral(std::string_view);
    ClassicPrettyPrinter& PutDoubleLiteral(double);
    ClassicPrettyPrinter& PutIntegerLiteral(long);
    ClassicPrettyPrinter& OnNewLine();
|] $++$ nest 4 (printMethodDecls printable) $+$ text "")
      printable
      False
      (unlinesToText [s|

// Calls PutToken
|])
      packwrap

    cpp = disclaimer $++$ unlinesToText [s|
#include "ClassicPrettyPrinter.hpp"

#include "PrinterCommon.hpp"
|] $++$ packwrap
      (printerUtilImpl
      $++$ vcatSpaced (map (makePutMethod listItemStorage) printable)
      $++$ vcatSpaced (map makeCallOperator printable)
      $++$ makeShlImpls printable
      )

------------------------------------------------------------------------
-- * Code generation.
------------------------------------------------------------------------

-- | Generates a list of @Put(...);@ method declarations.
printMethodDecls ::
     [PrintableSymbol]  -- ^ Classes to print.
  -> Doc
printMethodDecls = foldr (($+$) . text . printMethodDecl) empty

-- | Generates an instance method declaration of @Put(...);@.
printMethodDecl ::
     PrintableSymbol  -- ^ A class to print.
  -> String
printMethodDecl cls
  = "ClassicPrettyPrinter& Put(const " ++ printableClassName cls ++ "&"
    ++ maybeCoerc ++ ");"
  where
    maybeCoerc = case cls of
      PrintableNormalCategory _ -> ", int coercionLevel"
      _                         -> ""

-- | A notice to the user about the printing rules.
disclaimer :: Doc
disclaimer = unlinesToText [s|
/*
The ClassicPrettyPrinter class operates on tokens. Each syntax node is converted
into a sequence of tokens, which are then printed in order, separated by spaces.
However, some tokens have special effects:

** Space-separation **
    - Left brackets '[' and parentheses '(' eat the separating space after
      themselves.
    - Commas ',', semicolons ';', and right brackets ']' and parentheses ')'
      eat the separating space before themselves.
    - Tokens that start and/or end with whitespace eat the separating space on
      the whitespace side(s).
    - Empty tokens prevent spacing.

** Layout **
    - Left curly braces '{' are always printed on a separate line and increase
      the indentation level. The brace itself is _not_ indented.
    - Right curly braces '}' are always printed on a separate line and decrease
      the indentation level. The brace itself is _also_ de-dented.
    - Semicolons ';' cause a line break after themselves.

Precedence is always resolved by enclosing a term in parentheses '()'.

You are encouraged to _PATCH(1)_ this class or the ContextFreePrettyPrinter
class to make the printed text prettier.
*/
|]

-- | Implementations of helper methods.
printerUtilImpl :: Doc
printerUtilImpl = unlinesToText [s|
#define IF_BAD_COERC \
    if (reflection::CoercionLevel<std::decay_t<decltype(v)>> < coercionLevel)

static void PutLinebreak(std::ostream& out, unsigned int indent) {
    out << '\n' << std::string(indent, ' ');
}

ClassicPrettyPrinter::ClassicPrettyPrinter(
    std::ostream& out, unsigned int tabSize)
    : out(out), tabSize(tabSize) {}

ClassicPrettyPrinter& ClassicPrettyPrinter::FlushHere() {
    if (needNewline) {
        PutLinebreak(out, indentInTabs * tabSize);
        needNewline = false;
        needSpace = false;
    } else if (needSpace) {
        out << ' ';
        needSpace = false;
    }
    return *this;
}

ClassicPrettyPrinter& ClassicPrettyPrinter::SimplePut(std::string_view s) {
    FlushHere();
    out << s;
    needSpace = true;
    return *this;
}

unsigned int ClassicPrettyPrinter::TabSize() const {
    return tabSize;
}

unsigned int ClassicPrettyPrinter::IndentationLevel() const {
    return indentInTabs;
}

int ClassicPrettyPrinter::CoercionLevel() const {
    return coercionLevel;
}

ClassicPrettyPrinter& ClassicPrettyPrinter::WithTabSize(unsigned int tabSize) {
    this->tabSize = tabSize;
    return *this;
}

ClassicPrettyPrinter& ClassicPrettyPrinter::Indent(unsigned int tabs) {
    indentInTabs += tabs;
    return *this;
}

ClassicPrettyPrinter& ClassicPrettyPrinter::Dedent(unsigned int tabs) {
    if (tabs > indentInTabs)
        indentInTabs = 0;
    else
        indentInTabs -= tabs;
    return *this;
}

ClassicPrettyPrinter& ClassicPrettyPrinter::WithIndent(unsigned int tabs) {
    indentInTabs = tabs;
    return *this;
}

ClassicPrettyPrinter& ClassicPrettyPrinter::SkipSpaceHere() {
    needSpace = false;
    return *this;
}

ClassicPrettyPrinter& ClassicPrettyPrinter::NeedSpaceHere() {
    needSpace = true;
    return *this;
}

static bool IsSpace(char ch) {
    switch (ch) {
        case ' ': case '\r': case '\n': case '\t': case '\v':
            return true;
        default:
            return false;
    }
}

ClassicPrettyPrinter& ClassicPrettyPrinter::PutToken(std::string_view s) {
    if (s.empty()) {
        SkipSpaceHere();
        return *this;
    }
    if (s.size() == 1) {
        bool handled = true;
        switch (s.front()) {
            case '{':
                OnNewLine()
                    .SimplePut(s)
                    .Indent()
                    .OnNewLine();
                break;
            case '}':
                OnNewLine()
                    .Dedent()
                    .SimplePut(s)
                    .OnNewLine();
                break;
            case ';':
                SkipSpaceHere()
                    .SimplePut(s)
                    .OnNewLine();
                break;
            case ',': case ')': case ']':
                SkipSpaceHere()
                    .SimplePut(s)
                    .NeedSpaceHere();
                break;
            case '(': case '[':
                SimplePut(s)
                    .SkipSpaceHere();
                break;
            default:
                handled = false;
                break;
        }
        if (handled) return *this;
    }
    if (IsSpace(s.front())) SkipSpaceHere();
    SimplePut(s);
    if (IsSpace(s.back())) SkipSpaceHere();
    return *this;
}

ClassicPrettyPrinter& ClassicPrettyPrinter::PutCharLiteral(int32_t ch) {
    FlushHere();
    PrintEscapedChar(out, ch);
    return NeedSpaceHere();
}

ClassicPrettyPrinter& ClassicPrettyPrinter::PutStringLiteral(
        std::string_view s) {
    FlushHere();
    PrintEscapedString(out, s);
    return NeedSpaceHere();
}

ClassicPrettyPrinter& ClassicPrettyPrinter::PutDoubleLiteral(double v) {
    FlushHere();
    PrintDouble(out, v);
    return NeedSpaceHere();
}

ClassicPrettyPrinter& ClassicPrettyPrinter::PutIntegerLiteral(long v) {
    FlushHere();
    PrintDouble(out, v);
    return NeedSpaceHere();
}

ClassicPrettyPrinter& ClassicPrettyPrinter::OnNewLine() {
    needNewline = true;
    needSpace = false;
    return *this;
}
|]

-- | Generates a method that prints objects of the given type.
makePutMethod ::
     ListItemStorage  -- ^ How to access list elements.
  -> PrintableSymbol  -- ^ What object to print.
  -> Doc
makePutMethod listItemStorage = \case
  PrintableNormalCategory s -> methodCategory s
  PrintableList listDesc    -> methodList listItemStorage listDesc
  PrintableFunctionRule r   -> methodFunctionRule r
  PrintableCustomToken t    -> methodCustomToken t
  PrintableIdent            -> methodIdent
  PrintableString           -> methodString
  PrintableInteger          -> methodInteger
  PrintableChar             -> methodChar
  PrintableDouble           -> methodDouble

-- | Generates an operator() that prints objects of the given type.
makeCallOperator ::
     PrintableSymbol  -- ^ What object to print.
  -> Doc
makeCallOperator cls = linesToText
  [ concat
    [ "void ClassicPrettyPrinter::operator()(const "
    , printableClassName cls
    , "& v) {"
    ]
  , concat
    [ "    Put(v"
    , case cls of
      PrintableNormalCategory _ -> ", 0"
      _                         -> ""
    , ");"
    ]
  , "}"
  ]

------------------------------------------------------------------------
-- * Utility for code generation.
------------------------------------------------------------------------

-- | Method that pretty-prints an @Ident@.
methodIdent :: Doc
methodIdent = unlinesToText [s|
ClassicPrettyPrinter& ClassicPrettyPrinter::Put(const Ident& v) {
    return PutToken(v.Value);
}
|]

-- | Method that pretty-prints a @String@ token.
methodString :: Doc
methodString = unlinesToText [s|
ClassicPrettyPrinter& ClassicPrettyPrinter::Put(const String& v) {
    return PutStringLiteral(v.Value);
}
|]

-- | Method that pretty-prints an @Integer@ token.
methodInteger :: Doc
methodInteger = unlinesToText [s|
ClassicPrettyPrinter& ClassicPrettyPrinter::Put(const Integer& v) {
    return PutIntegerLiteral(v.Value);
}
|]

-- | Method that pretty-prints a @Double@ token.
methodDouble :: Doc
methodDouble = unlinesToText [s|
ClassicPrettyPrinter& ClassicPrettyPrinter::Put(const Double& v) {
    return PutDoubleLiteral(v.Value);
}
|]

-- | Method that pretty-prints a @Char@ token.
methodChar :: Doc
methodChar = unlinesToText [s|
ClassicPrettyPrinter& ClassicPrettyPrinter::Put(const Char& v) {
    return PutCharLiteral(v.Value);
}
|]

-- | Generates a method that pretty-prints a value of the given nonterminal.
methodCategory ::
     String  -- ^ The category name.
  -> Doc
methodCategory name = linesToText
  [ concat
    ["ClassicPrettyPrinter& ClassicPrettyPrinter::Put(const "
    , name
    , "& v, int coercLvl) {"
    ]
    , "    int cur = coercionLevel;"
    , "    coercionLevel = coercLvl;"
    , "    std::visit(*this, v);"
    , "    coercionLevel = cur;"
    , "    return *this;"
  , "}"
  ]

-- | Generates a method that pretty-prints an object built from the given
-- grammar rule.
methodFunctionRule :: CF.Rule -> Doc
methodFunctionRule r = linesToText
  [ concat
    [ "ClassicPrettyPrinter& ClassicPrettyPrinter::Put(const "
    , name
    , "& v [[maybe_unused]]) {"
    ]
  , "    IF_BAD_COERC PutToken(\"(\");"
  ] $+$ nest 4 (putSentForm $ CF.rhsRule r)
  $+$ linesToText
  [ "    IF_BAD_COERC PutToken(\")\");"
  , "    return *this;"
  , "}"
  ]
  where
    name = CF.funName r
    putSentForm :: CF.SentForm -> Doc
    putSentForm sent = linesToText $ putSentFormTail sent
      $ map fst $ fieldNames sent
    putSentFormTail :: CF.SentForm -> [String] -> [String]
    putSentFormTail sf fields = case sf of
      [] -> []
      Left cat : sfTail ->
        let
          fieldName : fieldsTail = fields
          resTail = putSentFormTail sfTail fieldsTail
        in case cat of
          CF.Cat _ -> ("Put(*v." ++ fieldName ++ ", 0);") : resTail
          CF.CoercCat _ lvl ->
            ("Put(*v." ++ fieldName ++ ", " ++ show lvl ++ ");") : resTail
          CF.TokenCat _ -> ("Put(v." ++ fieldName ++ ");") : resTail
          CF.ListCat _ -> ("Put(v." ++ fieldName ++ ");") : resTail
      Right s : sfTail ->
        ("PutToken(" ++ cppShowString s ++ ");") : putSentFormTail sfTail fields

-- | Generates a method that prints a custom token.
methodCustomToken :: String -> Doc
methodCustomToken name = linesToText
  [ "ClassicPrettyPrinter& ClassicPrettyPrinter::Put(const "
    ++ name ++ "& v) {"
  , "    IF_BAD_COERC PutToken(\"(\");"
  , "    PutToken(v." ++ tokenStorageName name ++ ");"
  , "    IF_BAD_COERC PutToken(\")\");"
  , "    return *this;"
  , "}"
  ]

-- | Generates a method that prints a list category.
methodList ::
     ListItemStorage           -- ^ How to access list elements.
  -> PrintableListDescription  -- ^ About the list.
  -> Doc
methodList listItemStorage (PrintableListDescription
  { printListName      = name
  , printListItemCoerc = itemcoerc
  , printListEmpty     = empty
  , printListCons      = cons
  , printListSingle    = single
  , printListOfVars    = containsVariants
  })
  = linesToText
  [ "ClassicPrettyPrinter& ClassicPrettyPrinter::Put(const " ++ name ++ "& v) {"
  , "    IF_BAD_COERC PutToken(\"(\");"
  , "    int prevCoerc = coercionLevel;"
  , "    coercionLevel = " ++ show itemcoerc ++ ";"
  ] $+$ nest 4 body $+$ linesToText
  [ "    coercionLevel = prevCoerc;"
  , "    IF_BAD_COERC PutToken(\")\");"
  , "    return *this;"
  , "}"
  ]
  where
    maybeDereference = case listItemStorage of
      StoreByValue   -> ""
      StoreByPointer -> "*"
    putItem
      | containsVariants =
        \ s -> "std::visit(*this, " ++ maybeDereference ++ s ++ ");"
      | otherwise        = \ s -> "Put(" ++ maybeDereference ++ s ++ ");"
    body = case single of
      Nothing ->
        text "for (const auto& item : v) {"
        $+$ nest 4 (lcons'
            $+$ text (putItem $ "item")
            $+$ mcons')
        $+$ text "}" $+$ empty' $+$ cyclercons False
      Just (lsingle, _, rsingle) -> text "if (v.empty()) {"
        $+$ nest 4 empty'
        $+$ text "} else {"
        $+$ nest 4 (
          text "auto last = std::prev(v.cend());"
          $+$ text "for (auto i = v.cbegin(); i != last; ++i) {"
          $+$ nest 4 (lcons'
              $+$ text (putItem "*i")
              $+$ mcons')
          $+$ text "}"
          $+$ lsingle'
          $+$ text (putItem "*last")
          $+$ rsingle'
          $+$ cyclercons True
        ) $+$ text "}"
        where
          lsingle' = compileSepString lsingle
          rsingle' = compileSepString rsingle
    compileSepString :: [String] -> Doc
    compileSepString strs = linesToText $ map printthis strs
      where
        printthis s = "PutToken(" ++ cppShowString s ++ ");"
    (lcons, mcons, rcons) = case cons of
      Nothing -> ([], [], [])
      Just (a, _, b, _, c) -> (a, b, c)
    lcons' = compileSepString lcons
    mcons' = compileSepString mcons
    rcons' = compileSepString rcons
    empty' = case empty of
      Nothing -> text "// Empty list not defined in syntax"
      Just ss -> compileSepString ss
    cyclercons needDecr = if isEmpty rcons' then rcons' else
      text ("for (size_t i = v.size()"
        ++ (if needDecr then " - 1" else "") ++ "; i; --i) {")
      $+$ nest 4 rcons' $+$ text "}"

-- | Generates implementations of operator<<. The code is specific to this
-- printer, so we cannot use
-- 'BNFC.Backend.CPPVar.PrinterUtils.makePrinterShlImplementations'.
makeShlImpls ::
     [PrintableSymbol]
  -> Doc
makeShlImpls symbols = unlinesToText [s|
#define ClassicPrettyPrinterSHL(type)                                          \
    ClassicPrettyPrinter& operator<<(ClassicPrettyPrinter& p, const type& v) { \
        return p.Put(v);                                                       \
    }

#define ClassicPrettyPrinterSHL0(type)                                         \
    ClassicPrettyPrinter& operator<<(ClassicPrettyPrinter& p, const type& v) { \
        return p.Put(v, 0);                                                    \
    }
|]
  $++$ linesToText
    [ concat
      [ "ClassicPrettyPrinterSHL"
      , case sym of
        PrintableNormalCategory _ -> "0"
        _ -> ""
      , "("
      , printableClassName sym
      , ");"
      ]
    | sym <- symbols
    ]
  $++$ unlinesToText [s|
ClassicPrettyPrinter& operator<<(ClassicPrettyPrinter& p, std::string_view v) {
    return p.PutToken(v);
}
|]
