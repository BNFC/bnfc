{-# LANGUAGE QuasiQuotes #-}

{-|
  Module      : BNFC.Backend.CPPVar.HaskellPrinterGen
  Description : Printing of the AST as a Haskell expression.

  Printing of the abstract syntax tree as a Haskell expression.
-}

module BNFC.Backend.CPPVar.HaskellPrinterGen
  (
    -- * The entrypoint
    makeHaskellPrinter

    -- * File naming
  , haskellPrinterHppFilename
  , haskellPrinterCppFilename
  ) where

-- Language imports
import Data.List (intersperse)
import Data.String.QQ (s)
import Text.PrettyPrint (($+$), Doc, nest, text)

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
haskellPrinterHppFilename :: String
haskellPrinterHppFilename = "HaskellPrinter.hpp"

-- | The name of the source file.
haskellPrinterCppFilename :: String
haskellPrinterCppFilename = "HaskellPrinter.cpp"

------------------------------------------------------------------------
-- * Code generation.
------------------------------------------------------------------------

makeHaskellPrinter ::
     Options.SharedOptions  -- ^ BNFC invokation options.
  -> (String -> Bool)       -- ^ Checks if a token tracks its position.
  -> [PrintableSymbol]      -- ^ The list of types to make methods for.
  -> ListItemStorage        -- ^ How to access list items.
  -> CPPHeaderSourcePair
makeHaskellPrinter opts isPosToken printable listItemStorage =
  CPPHeaderSourcePair
    { cppHeaderText = hpp
    , cppSourceText = cpp
    }
  where
    packwrap = nsutils_wrap $ newNamespaceUtilsFromOptions opts

    hpp = makePrinterHeaderFile "HaskellPrinter" (unlinesToText [s|
#pragma once
#include <iostream>
#include <string_view>
#include "Absyn.hpp"
|])
      (unlinesToText [s|
    std::ostream& out;
    bool inExpression;
    friend const HaskellPrinter& operator<<(const HaskellPrinter&,
                                           std::string_view);
    HaskellPrinter(std::ostream& out, bool inExpression);
    HaskellPrinter PrintConstructorArg() const;

public:
    explicit HaskellPrinter(std::ostream& out);
    void operator()(const location&) const;
    void operator()(const position&) const;
|])
      printable True (unlinesToText [s|
const HaskellPrinter& operator<<(const HaskellPrinter&, const location&);
const HaskellPrinter& operator<<(const HaskellPrinter&, const position&);
|]) packwrap

    cpp = text "#include \"HaskellPrinter.hpp\""
      $++$ text "#include \"PrinterCommon.hpp\""
      $++$ packwrap
        (printerImpl
          (getLocationKind opts /= CppLocationsNone)
          isPosToken listItemStorage printable)

-- | Generates the implementation.
printerImpl ::
     Bool               -- ^ Print positions?
  -> (String -> Bool)   -- ^ Checks if a token tracks its position.
  -> ListItemStorage    -- ^ How to access list elements.
  -> [PrintableSymbol]  -- ^ All symbols to generate methods for.
  -> Doc
printerImpl printPositions isPosToken listItemStorage symbols =
  unlinesToText [s|
HaskellPrinter::HaskellPrinter(std::ostream& out, bool inExpression)
    : out(out)
    , inExpression(inExpression) {}

HaskellPrinter::HaskellPrinter(std::ostream& out)
    : HaskellPrinter(out, false) {}

HaskellPrinter HaskellPrinter::PrintConstructorArg() const {
    return {out, true};
}

void HaskellPrinter::operator()(const location& loc) const {
    out << "((" << loc.begin.line << ',' << loc.begin.column << "),("
        << loc.end.line << ',' << loc.end.column << "))";
}

void HaskellPrinter::operator()(const position& pos) const {
    out << '(' << pos.line << ',' << pos.column << ')';
}
|]
  $++$ vcatSpaced
    (map (makeMethod printPositions isPosToken listItemStorage) symbols)
  $++$ makePrinterShlImplementations "HaskellPrinter"
    ( PrintableCustomToken "location"
    : PrintableCustomToken "position"
    : symbols
    )

-- | Generates an implementation of printing a class.
makeMethod ::
     Bool              -- ^ Print positions?
  -> (String -> Bool)  -- ^ Checks if a token tracks its position.
  -> ListItemStorage   -- ^ How to access list elements.
  -> PrintableSymbol   -- ^ A class to print.
  -> Doc
makeMethod printPositions isPosToken storeListItemsBy = \case
  PrintableNormalCategory name -> methodWrap True name
    $ text "std::visit(*this, v);"
  PrintableList PrintableListDescription { printListName = name } ->
    let
      derefItem = case storeListItemsBy of
        StoreByValue   -> ""
        StoreByPointer -> "*"
    in
      methodWrap True name $ linesToText
      [ "out << '[';"
      , "if (!v.empty()) {"
      , "    const HaskellPrinter printItem(out);"
      , "    auto last = std::prev(v.cend());"
      , "    for (auto i = v.cbegin(); i != last; ++i) {"
      , "        printItem(" ++ derefItem ++ "*i);"
      , "        out << ',';"
      , "    }"
      , "    printItem(" ++ derefItem ++ "*last);"
      , "}"
      , "out << ']';"
      ]
  PrintableFunctionRule rule -> let
      className = CF.funName rule
      fields = fieldNames $ CF.rhsRule rule
      body =
        if null fields && not printPositions
        then text ("out << \"" ++ className ++ "\";")
        else
          text "if (inExpression) out << '(';"
          $+$ printValueCtorName printPositions className
          $+$ text "const HaskellPrinter printField = PrintConstructorArg();"
          $+$ linesToText
            (intersperse "out << ' ';"
            [ concat
              [ "printField("
              , if isPointerCat fieldtype then "*" else ""
              , "v."
              , fieldname
              , ");"
              ]
            | (fieldname, fieldtype) <- fields])
          $+$ text "if (inExpression) out << ')';"
    in methodWrap (not (null fields)) className body
  PrintableCustomToken name -> customTokenMethod name
  PrintableIdent   -> customTokenMethod "Ident"
  PrintableString  ->
    methodWrap True "String" $ text ("PrintEscapedString(out, v.Value);")
  PrintableChar    ->
    methodWrap True "Char" $ text ("PrintEscapedChar(out, v.Value);")
  PrintableDouble  ->
    methodWrap True "Double" $ text ("PrintDouble(out, v.Value);")
  PrintableInteger -> methodWrap True "Integer" $ text ("out << v.Value;")
  where
    printValueCtorNameNoLoc name = text ("out << \"" ++ name ++ " \";")
    printValueCtorNameWithLoc name = linesToText
      [ "out << \"" ++ name ++ " (Just \";"
      , "(*this)(v.loc);"
      , "out << \") \";"
      ]
    printValueCtorName withLoc
      | withLoc   = printValueCtorNameWithLoc
      | otherwise = printValueCtorNameNoLoc
    customTokenMethod argStructName = methodWrap True argStructName
      $ text "if (inExpression) out << '(';"
      $+$ printValueCtorName
        (printPositions && isPosToken argStructName) argStructName
      $+$ text ("PrintEscapedString(out, v."
        ++ tokenStorageName argStructName ++ ");")
      $+$ text "if (inExpression) out << ')';"
    methodWrap argUsed name body =
      text (concat
        [ "void HaskellPrinter::operator()(const "
        , name
        , "&"
        , (if argUsed then " v" else "")
        , ") const {"
        ])
      $+$ nest 4 body
      $+$ text "}"
    isPointerCat = \case
      CF.TokenCat _   -> False
      CF.ListCat  _   -> False
      CF.CoercCat _ _ -> True
      CF.Cat      _   -> True
