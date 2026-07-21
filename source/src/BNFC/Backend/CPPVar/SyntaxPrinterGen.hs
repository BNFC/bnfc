{-# LANGUAGE QuasiQuotes #-}

{-|
  Module      : BNFC.Backend.CPPVar.SyntaxPrinterGen
  Description : Printing the abstract syntax tree as it is.

  Printing the abstract syntax tree as it is.
-}

module BNFC.Backend.CPPVar.SyntaxPrinterGen
  (
    -- * The entrypoint
    makeSyntaxPrinter

    -- * File naming
  , syntaxPrinterHppFilename
  , syntaxPrinterCppFilename
  ) where

-- Language imports
import Data.String.QQ (s)
import Text.PrettyPrint (($+$), Doc, empty, nest, text)

-- BNFC imports
import qualified BNFC.CF as CF
import qualified BNFC.Options as Options

import BNFC.Backend.CPPVar.AbsynGen (tokenStorageName, ListItemStorage(..))
import BNFC.Backend.CPPVar.CPPUtil
import BNFC.Backend.CPPVar.PrinterUtils

------------------------------------------------------------------------
-- * File naming.
------------------------------------------------------------------------

-- | The name of the header file.
syntaxPrinterHppFilename :: String
syntaxPrinterHppFilename = "SyntaxPrinter.hpp"

-- | The name of the source file.
syntaxPrinterCppFilename :: String
syntaxPrinterCppFilename = "SyntaxPrinter.cpp"

------------------------------------------------------------------------
-- * Code generation.
------------------------------------------------------------------------

-- | Generates the @SyntaxPrinter@ class (declaration and implementation).
makeSyntaxPrinter ::
     Options.SharedOptions  -- ^ BNFC invokation options.
  -> (String -> Bool)       -- ^ Checks if a token tracks its location.
  -> [PrintableSymbol]      -- ^ The list of types to make methods for.
  -> ListItemStorage        -- ^ How to access list items.
  -> CPPHeaderSourcePair
makeSyntaxPrinter opts isPosToken printable listItemStorage =
  CPPHeaderSourcePair
    { cppHeaderText = hpp
    , cppSourceText = cpp
    }
  where
    packwrap = nsutils_wrap $ newNamespaceUtilsFromOptions opts

    hpp = makePrinterHeaderFile "SyntaxPrinter" (unlinesToText [s|
#pragma once
#include <iostream>
#include <string_view>
#include "Absyn.hpp"
|])
      (unlinesToText [s|
    std::ostream& out;
    bool currentIndentIsBranch;
    const SyntaxPrinter* maybeParent;
    SyntaxPrinter(const SyntaxPrinter* parent, bool currentIndentIsBranch);
    friend const SyntaxPrinter& operator<<(const SyntaxPrinter&,
                                           std::string_view);
    void PrintIndentForHeader() const;
    void PrintIndentAsIs() const;

public:
    explicit SyntaxPrinter(std::ostream& out);
|])
      printable True empty packwrap

    cpp = text "#include \"SyntaxPrinter.hpp\""
      $++$ text "#include \"PrinterCommon.hpp\""
      $++$ packwrap
        (printerImpl
          (getLocationKind opts /= CppLocationsNone)
          isPosToken listItemStorage printable)

-- | Generates the implementation.
printerImpl ::
     Bool               -- ^ Print locations?
  -> (String -> Bool)   -- ^ Checks if a token tracks its location.
  -> ListItemStorage    -- ^ How to access list elements.
  -> [PrintableSymbol]  -- ^ All symbols to generate methods for.
  -> Doc
printerImpl printLocations isPosToken listItemStorage symbols =
  unlinesToText [s|
SyntaxPrinter::SyntaxPrinter(const SyntaxPrinter* parent,
                             bool currentIndentIsBranch)
    : out(parent->out),
      currentIndentIsBranch(currentIndentIsBranch),
      maybeParent(parent) {}

SyntaxPrinter::SyntaxPrinter(std::ostream& out)
    : out(out), currentIndentIsBranch(false), maybeParent(nullptr) {}

void SyntaxPrinter::PrintIndentForHeader() const {
    if (!maybeParent) return;
    maybeParent->PrintIndentAsIs();
    out << "+-";
}

void SyntaxPrinter::PrintIndentAsIs() const {
    if (!maybeParent) return;
    maybeParent->PrintIndentAsIs();
    out << (currentIndentIsBranch ? "| " : "  ");
}
|] $++$ vcatSpaced (map makeMethod symbols)
  $++$ makePrinterShlImplementations "SyntaxPrinter" symbols
  where
    maybePrintLoc isLast
      | printLocations = linesToText
        [ "PrintIndentAsIs();"
        , concat
          [ "out << \""
          , if isLast
            then " "
            else "|"
          , " @ \" << v.loc << '\\n';"
          ]
        ]
      | otherwise = empty
    maybeDereference = case listItemStorage of
      StoreByValue   -> ""
      StoreByPointer -> "*"
    makeMethod sym = text
      ("void SyntaxPrinter::operator()(const "
        ++ printableClassName sym ++  "& v [[maybe_unused]]) const {")
      $+$ nest 4 (makeMethodBody sym) $+$ text "}"
    makeMethodBody = \case
      PrintableNormalCategory _ -> text "std::visit(*this, v);"
      PrintableList
        (PrintableListDescription {printListName = name}) -> linesToText
        [ "PrintIndentForHeader();"
        , "size_t n = v.size();"
        , "out << \"" ++ name ++ " [\" << n << \"]\\n\";"
        ] $+$ (if printLocations
          then unlinesToText [s|
PrintIndentAsIs();
out << (n ? "| " : "  ");
out << "@ " << v.loc << '\n';
|]
          else empty
        ) $+$ linesToText
        [ "if (!n) return;"
        , "if (n > 1) {"
        , "    SyntaxPrinter nonlast(this, true);"
        , "    size_t n1 = n - 1;"
        , "    for (size_t i = 0; i < n1; i++)"
        , "        nonlast(" ++ maybeDereference ++ "v[i]);"
        , "}"
        , "SyntaxPrinter(this, false)(" ++ maybeDereference ++ "v.back());"
        ]
      PrintableCustomToken name -> stringlikePrint name $+$
          if isPosToken name
          then maybePrintLoc True
          else empty
      PrintableIdent            -> unlinesToText [s|
PrintIndentForHeader();
out << "Ident {" << v.Value << "}\n";
|]
      PrintableString           -> stringlikePrint "String"
      PrintableInteger          -> unlinesToText [s|
PrintIndentForHeader();
out << "Integer " << v.Value << '\n';
|]
      PrintableDouble           -> unlinesToText [s|
PrintIndentForHeader();
out << "Double ";
PrintDouble(out, v.Value);
out << '\n';
|]
      PrintableChar             -> unlinesToText [s|
PrintIndentForHeader();
out << "Char ";
PrintEscapedChar(out, v.Value);
out << '\n';
|]
      PrintableFunctionRule r   -> linesToText
        [ "PrintIndentForHeader();"
        , "out << \"" ++ CF.funName r ++ "\\n\";"
        ] $+$ case myUnsnoc (fieldNames $ CF.rhsRule r) of
          Nothing                              -> maybePrintLoc True
          Just (nonlasts, (lastName, lastCat)) ->
            maybePrintLoc False
            $+$ (case nonlasts of
              [] -> empty
              _ -> text "SyntaxPrinter nonlast(this, true);"
                $+$ linesToText
                [concat
                  [ "nonlast("
                  , if isPointerType cat then "*" else ""
                  , "v."
                  , name
                  , ");"
                  ]
                | (name, cat) <- nonlasts])
              $+$ text (concat
                [ "SyntaxPrinter(this, false)("
                , if isPointerType lastCat then "*" else ""
                , "v."
                , lastName
                , ");"
                ])
        where
          isPointerType = \case
            CF.Cat      _   -> True
            CF.CoercCat _ _ -> True
            _               -> False
          -- | @unsnoc@ is unavailable in old haskell.
          myUnsnoc :: [a] -> Maybe ([a], a)
          myUnsnoc = \case
            []          -> Nothing
            item : tail -> case myUnsnoc tail of
              Nothing           -> Just ([], item)
              Just (init, last) -> Just (item : init, last)
    stringlikePrint name = linesToText
      [ "PrintIndentForHeader();"
      , "out << \"" ++ name ++ " \";"
      , "PrintEscapedString(out, v." ++ tokenStorageName name ++ ");"
      , "out << '\\n';"
      ]
