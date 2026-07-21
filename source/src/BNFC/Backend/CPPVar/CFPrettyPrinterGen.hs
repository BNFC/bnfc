{-# LANGUAGE QuasiQuotes #-}

{-|
  Module      : BNFC.Backend.CPPVar.CFPrettyPrinterGen
  Description : Heuristic-driven context-free
                pretty-printing of the abstract syntax tree.

  Heuristic-driven context-free pretty-printing of the abstract syntax tree.
-}

module BNFC.Backend.CPPVar.CFPrettyPrinterGen
  (
    -- * The entrypoint
    makePrettyPrinter

    -- * File naming
  , prettyPrinterHppFilename
  , prettyPrinterCppFilename
  ) where

-- Language imports
import Data.Char (isSpace)
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
prettyPrinterHppFilename :: String
prettyPrinterHppFilename = prettyPrinterClassName ++ ".hpp"

-- | The name of the source file.
prettyPrinterCppFilename :: String
prettyPrinterCppFilename = prettyPrinterClassName ++ ".cpp"

------------------------------------------------------------------------
-- * The entrypoint.
------------------------------------------------------------------------

prettyPrinterClassName :: String
prettyPrinterClassName = "ContextFreePrettyPrinter"

-- | Generates the @ContextFreePrettyPrinter@ class
-- (declaration and implementation).
makePrettyPrinter ::
     BNFC.Options.SharedOptions  -- ^ BNFC invokation options.
  -> [PrintableSymbol]           -- ^ The list of types to make methods for.
  -> ListItemStorage             -- ^ How to access list elements.
  -> CPPHeaderSourcePair
makePrettyPrinter opts printable listItemStorage = CPPHeaderSourcePair
  { cppHeaderText = hpp
  , cppSourceText = cpp
  }
  where
    NamespaceUtils { nsutils_wrap = packwrap } =
      newNamespaceUtilsFromOptions opts

    hpp = makePrinterHeaderFile prettyPrinterClassName (unlinesToText [s|
// The default context-free pretty printer does not suit all languages and is
// meant to be modified. See ContextFreePrettyPrinter.cpp for details.

#pragma once
#include <iostream>
#include <string_view>
#include "Absyn.hpp"
|])
      (unlinesToText [s|
    std::ostream& out;
    unsigned int indent;
    int coercionLevel;
    void NewLine(unsigned int indent) const;

    friend const ContextFreePrettyPrinter& operator<<(
        const ContextFreePrettyPrinter&, std::string_view);

public:
    ContextFreePrettyPrinter(std::ostream&, unsigned int indent = 0,
                             int coercionLevel = 0);
    ContextFreePrettyPrinter Indented(unsigned int plusIndent = 4,
                                      int coercionLevel = 0) const;
    ContextFreePrettyPrinter Dedented(unsigned int minusIndent = 4,
                                      int coercionLevel = 0) const;
    ContextFreePrettyPrinter WithCoercionLevel(int level) const;
    void NewLine() const;
|])
      printable True empty packwrap

    cpp = disclaimer $++$ unlinesToText [s|
#include "ContextFreePrettyPrinter.hpp"

#include "PrinterCommon.hpp"
|] $++$ packwrap
      (printerUtilImpl
      $++$ vcatSpaced (map (makeMethod listItemStorage) printable)
      $++$ makePrinterShlImplementations prettyPrinterClassName printable)

------------------------------------------------------------------------
-- * Code generation.
------------------------------------------------------------------------

-- | A notice to the user about the limitations.
disclaimer :: Doc
disclaimer = unlinesToText [s|
/********************************  Disclaimer  *********************************

The ContextFreePrettyPrinter class attempts to implement pretty-printing while
having as little internal state as possible, only depending on the indentation
level and the expected precedence level of the printed syntax subtree. The
minimal state should help the programmer modify the pretty-printer.

The default ContextFreePrettyPrinter implementation tries to follow these rules:

* All tokens are separated from each other by a space. The exceptions are:
  - commas ',' and semicolons ';', which are only separated from the right;
  - brackets '[]' and parentheses '()', which are not separated from the
    enclosed text;
  - tokens that start and/or end with whitespace, which are not separated on the
    whitespace side(s);
  - empty tokens prevent space-separation.

* Curly braces '{}' (and only those) enclose an indented block. The indentation
  is controlled by a global constant INDENT and equals 4 spaces by default. The
  left curly brace causes one line break _after_ itself, the right one causes
  breaks _before and after_ itself. The right brace is not indented.

  NB: brace pairs are only detected within one syntax node. Braces without pairs
  are treated like parentheses (not space-separated from what is inside).

* Semicolons ';' cause a line break after themselves.

* Precedence is always resolved by enclosing a term in parentheses '()'.

Note that only tokens specified in the grammar as literal strings affect
spacing, indentation, and line breaks in the generated implementation; tokens
defined using pragmas do not.

You are encouraged to _PATCH(1)_ this file with your own implementations of some
methods. If that is unacceptable and the printed text is not pretty, try
ClassicPrettyPrinter.

*******************************************************************************/
|]

-- | Implementations of helper methods.
printerUtilImpl :: Doc
printerUtilImpl = unlinesToText [s|
#define IF_BAD_COERC(classname) \
    if (coercionLevel > reflection::CoercionLevel<classname>)

constexpr static const unsigned int INDENT [[maybe_unused]] = 4;

ContextFreePrettyPrinter::ContextFreePrettyPrinter(
    std::ostream& out, unsigned int indent, int coercionLevel)
    : out(out), indent(indent), coercionLevel(coercionLevel) {}

void ContextFreePrettyPrinter::NewLine(unsigned int indent) const {
    out << '\n' << std::string(indent, ' ');
}

void ContextFreePrettyPrinter::NewLine() const { NewLine(indent); }

ContextFreePrettyPrinter
    ContextFreePrettyPrinter::WithCoercionLevel(int level) const {
    return {out, indent, level};
}

ContextFreePrettyPrinter ContextFreePrettyPrinter::Indented(
    unsigned int plusIndent, int coercionLevel) const {
    return {out, indent + plusIndent, coercionLevel};
}

ContextFreePrettyPrinter ContextFreePrettyPrinter::Dedented(
    unsigned int minusIndent, int coercionLevel) const {
    return {out, minusIndent > indent ? 0 : indent - minusIndent,
            coercionLevel};
}
|]

-- | Generates a method that prints objects of the given type.
makeMethod ::
     ListItemStorage  -- ^ How to access list elements.
  -> PrintableSymbol  -- ^ What object to print.
  -> Doc
makeMethod listItemStorage = \case
  PrintableNormalCategory s -> methodCategory s
  PrintableList listDesc    -> methodList listItemStorage listDesc
  PrintableFunctionRule r   -> methodFunctionRule r
  PrintableCustomToken t    -> methodCustomToken t
  PrintableIdent            -> methodIdent
  PrintableString           -> methodString
  PrintableInteger          -> methodInteger
  PrintableChar             -> methodChar
  PrintableDouble           -> methodDouble

------------------------------------------------------------------------
-- * Utility for code generation.
------------------------------------------------------------------------

-- | Internal structure; represents instructions to print an object.
data PrintTerm
  = Str         !String               -- ^ A literal string to print verbatim.
  | Nest        ![PrintTerm]          -- ^ Contents are to be printed indented.
  | Newline                           -- ^ Translated to a line break.
  | Nonterminal !NonterminalPrintTerm -- ^ A member field.

-- | Information about a printable field.
data NonterminalPrintTerm = NonterminalPrintTerm
  { printNonterm_coercionLevel :: !Integer  -- ^ The expected precedence.
  , printNonterm_fieldName     :: !String
  , printNonterm_isPointer     :: !Bool     -- ^ Is this a @std::unique_ptr@?
  }

-- | Step 0.
-- Generates printing instructions from a grammar rule's right-hand side.
getBasicPrintTerms ::
     CF.SentForm  -- ^ Representation of an AST class in the grammar.
  -> [PrintTerm]
getBasicPrintTerms sentForm = helper (map fst $ fieldNames sentForm) sentForm
  where
    helper fields = helper'
      where
        helper' = \case
          []       -> []
          h : tail -> case h of
            Left cat ->
              let f : fs = fields
              in Nonterminal (NonterminalPrintTerm
                { printNonterm_coercionLevel = case cat of
                    CF.CoercCat _ c -> c
                    _ -> 0
                , printNonterm_fieldName     = f
                , printNonterm_isPointer     = case cat of
                    CF.CoercCat _ _ -> True
                    CF.Cat      _   -> True
                    _               -> False
                }) : helper fs tail
            Right s  -> Str s : helper' tail

-- | Step 1.
-- Put sequences of terms enclosed in curly braces in v'Nest' terms.
handleCurlyBraces :: [PrintTerm] -> [PrintTerm]
handleCurlyBraces lst =
  let (read, unread) = helper lst
  in case unread of
    []               -> read
    unmatched : tail -> concat [read, [unmatched], handleCurlyBraces tail]
  where
    -- | Reads until the first unbalanced }.
    -- Returns (processed prefix, unprocessed suffix)
    helper :: [PrintTerm] -> ([PrintTerm], [PrintTerm])
    helper = \case
      []             -> ([], [])
      lst@(t : tail) -> case t of
        Str "{" ->
          let (inside, outside) = helper tail
          in case outside of
            closing@(Str "}"):tail ->
              let (read, unread) = helper tail
              in
                ( t : Nest (Newline : inside) : Newline
                  : closing : Newline : read
                , unread
                )
            _                      -> (t : inside, outside)
        Str "}" -> ([], lst)
        _       ->
          let (read, unread) = helper tail
          in (t : read, unread)

-- | Step 2.
-- Puts spaces between certain tokens (see 'disclaimer').
handleTokenSpacing :: [PrintTerm] -> [PrintTerm]
handleTokenSpacing = \case
  []           -> []
  first : tail -> handleNested first : helper first tail
  where
    handleNested = \case
      Nest nested -> Nest $ handleTokenSpacing nested
      notNested   -> notNested
    helper :: PrintTerm -> [PrintTerm] -> [PrintTerm]
    helper = \case
      Str ";" -> (Newline :) . helper Newline
      prev    -> \case
        []         -> []
        cur : tail ->
          (if needSep prev cur then (space :) else id)
          (handleNested cur : helper cur tail)
    needSep :: PrintTerm -> PrintTerm -> Bool
    needSep a b = case a of
      Newline -> False
      Str s   -> (s `notElem` ["{", "[", "("]) && not (endsInSpace s) && right
      _       -> right
      where
        right = case b of
          Newline -> False
          Str s   ->
            s `notElem` ["}", "]", ")", ",", ";"] && (not $ startsWithSpace s)
          _       -> True
    space = Str " "
    endsInSpace = \case
      ""       -> True
      [ch]     -> isSpace ch
      _ : tail -> endsInSpace tail
    startsWithSpace = \case
      ""       -> True
      head : _ -> isSpace head

-- | Step 3.
-- Merges consequent v'Str' terms into one.
-- Also removes empty strings from the result.
mergeStrs :: [PrintTerm] -> [PrintTerm]
mergeStrs = map (\case
    Left ss -> Str (concat ss)
    Right t -> handleNested t
  ) . helper
  where
    handleNested = \case
      Nest nested -> Nest $ mergeStrs nested
      notNested   -> notNested
    helper :: [PrintTerm] -> [Either [String] PrintTerm]
    helper = \case
      []       -> []
      h : tail -> let tail' = helper tail in
        case h of
          Str "" -> tail'
          Str sh -> case tail' of
            (Left ss) : ttail' -> Left (sh : ss) : ttail'
            _                  -> Left [sh] : tail'
          _      -> Right h : tail'

-- | Method that pretty-prints an @Ident@.
methodIdent :: Doc
methodIdent = unlinesToText [s|
void ContextFreePrettyPrinter::operator()(const Ident& v) const {
    IF_BAD_COERC(Ident) out << '(';
    out << v.Value;
    IF_BAD_COERC(Ident) out << ')';
}
|]

-- | Method that pretty-prints a @String@ token.
methodString :: Doc
methodString = unlinesToText [s|
void ContextFreePrettyPrinter::operator()(const String& v) const {
    IF_BAD_COERC(String) out << '(';
    PrintEscapedString(out, v.Value);
    IF_BAD_COERC(String) out << ')';
}
|]

-- | Method that pretty-prints an @Integer@ token.
methodInteger :: Doc
methodInteger = unlinesToText [s|
void ContextFreePrettyPrinter::operator()(const Integer& v) const {
    IF_BAD_COERC(Integer) out << '(';
    out << v.Value;
    IF_BAD_COERC(Integer) out << ')';
}
|]

-- | Method that pretty-prints a @Double@ token.
methodDouble :: Doc
methodDouble = unlinesToText [s|
void ContextFreePrettyPrinter::operator()(const Double& v) const {
    IF_BAD_COERC(Double) out << '(';
    PrintDouble(out, v.Value);
    IF_BAD_COERC(Double) out << ')';
}
|]

-- | Method that pretty-prints a @Char@ token.
methodChar :: Doc
methodChar = unlinesToText [s|
void ContextFreePrettyPrinter::operator()(const Char& v) const {
    IF_BAD_COERC(Char) out << '(';
    PrintEscapedChar(out, v.Value);
    IF_BAD_COERC(Char) out << ')';
}
|]

-- | Generates a method that pretty-prints a value of the given nonterminal.
methodCategory ::
     String  -- ^ The category name.
  -> Doc
methodCategory name = linesToText
  [ "void ContextFreePrettyPrinter::operator()(const " ++ name ++ "& v) const {"
  , "    std::visit(*this, v);"
  , "}"
  ]

-- | Generates a method that pretty-prints an object built from the given
-- grammar rule.
methodFunctionRule :: CF.Rule -> Doc
methodFunctionRule r = linesToText
  [ "void ContextFreePrettyPrinter::operator()(const " ++ name
    ++ "& v [[maybe_unused]]) const {"
  , "    IF_BAD_COERC(" ++ name ++ ") out << '(';"
  ] $+$ nest 4 (fst $ helperTerm2doc 0 terms)
  $+$ linesToText
  [ "    IF_BAD_COERC(" ++ name ++ ") out << ')';"
  , "}"
  ]
  where
    name = CF.funName r
    terms = mergeStrs $ handleTokenSpacing $ handleCurlyBraces
      $ getBasicPrintTerms $ CF.rhsRule r
    -- | returns (Doc, Have we used the new printer object?)
    nestedTerm2doc :: Int -> [PrintTerm] -> (Doc, Bool)
    nestedTerm2doc lv terms =
      let (doc, uses) = helperTerm2doc lv terms
      in ((if uses
            then text ("ContextFreePrettyPrinter printer" ++ show lv ++ " = " ++
              printerDotAtLv (lv - 1) ++ "Indented(INDENT);")
            else empty)
          $+$ doc
        , uses)
    -- | returns (Doc, Have we used the current printer object?)
    helperTerm2doc :: Int -> [PrintTerm] -> (Doc, Bool)
    helperTerm2doc lv = \case
      []          -> (empty, False)
      term : tail -> case term of
        Str s   ->
          ( text ("out << " ++ cppShowString s ++ ";") $+$ taildoc
          , tailUsesPrinter)
        Newline -> (text (printerDot ++ "NewLine();") $+$ taildoc, True)
        Nest ns ->
          let (nestdoc, nestUsesPrinter) = nestedTerm2doc (lv + 1) ns
          in (text "{" $+$ nest 4 nestdoc $+$ text "}" $+$ taildoc
            , nestUsesPrinter || tailUsesPrinter)
        Nonterminal (NonterminalPrintTerm
          { printNonterm_coercionLevel = coerc
          , printNonterm_fieldName     = field
          , printNonterm_isPointer     = isPointer
          }) -> (text (concat
            [ printerDot
            , "WithCoercionLevel("
            , show coerc
            , ")("
            , if isPointer then "*v." else "v."
            , field
            , ");"
            ]) $+$ taildoc, True)
        where
          (taildoc, tailUsesPrinter) = helperTerm2doc lv tail
          printerDot = printerDotAtLv lv
    printerDotAtLv = \case
      0 -> ""
      i -> "printer" ++ show i ++ "."

-- | Generates a method that prints a custom token.
methodCustomToken :: String -> Doc
methodCustomToken name = linesToText
  [ "void ContextFreePrettyPrinter::operator()(const " ++ name ++ "& v) const {"
  , "    IF_BAD_COERC(" ++ name ++ ") out << '(';"
  , "    out << v." ++ tokenStorageName name ++ ";"
  , "    IF_BAD_COERC(" ++ name ++ ") out << ')';"
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
  })
  = linesToText
  [ "void ContextFreePrettyPrinter::operator()(const " ++ name ++ "& v) const {"
  , "    IF_BAD_COERC(" ++ name ++ ") out << '(';"
  ] $+$ nest 4 body $+$ linesToText
  [ "    IF_BAD_COERC(" ++ name ++ ") out << ')';"
  , "}"
  ]
  where
    maybeDereference = case listItemStorage of
      StoreByValue   -> ""
      StoreByPointer -> "*"
    body = case single of
      Nothing -> itemprinter
        $+$ text "for (const auto& item : v) {"
        $+$ nest 4 (lcons'
            $+$ text ("itemprinter(" ++ maybeDereference ++ "item);")
            $+$ mcons')
        $+$ text "}" $+$ empty' $+$ cyclercons False
      Just (lsingle, _, rsingle) -> text "if (v.empty()) {"
        $+$ nest 4 empty'
        $+$ text "} else {"
        $+$ nest 4 (itemprinter
          $+$ text "auto last = std::prev(v.cend());"
          $+$ text "for (auto i = v.cbegin(); i != last; ++i) {"
          $+$ nest 4 (lcons'
              $+$ text ("itemprinter(" ++ maybeDereference ++ "*i);")
              $+$ mcons')
          $+$ text "}"
          $+$ lsingle'
          $+$ text ("itemprinter(" ++ maybeDereference ++ "*last);")
          $+$ rsingle'
          $+$ cyclercons True
        ) $+$ text "}"
        where
          lsingle' = compileSepString (lsingle ++ [""])
          rsingle' = compileSepString ("" : rsingle)

    itemprinter = text
      $ "ContextFreePrettyPrinter itemprinter = WithCoercionLevel("
      ++ show itemcoerc ++ ");"
    compileSepString :: [String] -> Doc
    compileSepString strs =
      linesToText $ map printthis $ mergeStrs $ handleTokenSpacing
          $ getBasicPrintTerms $ map Right strs
      where
        printthis = \case
          Str s         -> "out << " ++ cppShowString s ++ ";"
          Newline       -> "NewLine();";
          Nest _        -> error "Somehow got nesting in list"
          Nonterminal _ -> error "Somehow got categories in separators"
    (lcons, mcons, rcons) = case cons of
      Nothing -> ([], [], [])
      Just (a, _, b, _, c) -> (a, b, c)
    lcons' = compileSepString $ lcons ++ [""]
    mcons' = compileSepString $ "" : (mcons ++ [""])
    rcons' = compileSepString $ "" : rcons
    empty' = case empty of
      Nothing -> text "// Empty list not defined in syntax"
      Just ss -> compileSepString ss
    cyclercons needDecr = if isEmpty rcons' then rcons' else
      text ("for (size_t i = v.size()"
        ++ (if needDecr then " - 1" else "") ++ "; i; --i) {")
      $+$ nest 4 rcons' $+$ text "}"
