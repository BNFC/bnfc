{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module BNFC.Backend.Swift.CFtoSwiftAST (cf2SwiftAST) where

import Data.Maybe (mapMaybe)
import Data.List (intercalate, intersperse)
import Text.PrettyPrint.HughesPJClass (Doc, text, vcat, nest, ($$))

import BNFC.CF
import BNFC.Utils ((+++))
import BNFC.Backend.Swift.Common
import BNFC.Backend.Common.NamedVariables (UserDef)

-- | Produces abstract data types in Swift
cf2SwiftAST :: String -> CF -> Doc
cf2SwiftAST langName cf = vcat
    [ imports
    , empty
    , vcat (intersperse empty (map mkTokenDecl allTokenNames))
    , empty
    , vcat (intersperse empty (concatMap prData rules))
    ]
  where
    empty = text ""
    rules = getAbstractSyntax cf
    imports = vcat [text "import Foundation"]
    allTokenNames = literals cf

    -- | Generates a Swift struct for a token.
    mkTokenDecl :: String -> Doc
    mkTokenDecl tokenName = vcat
        [ text $ "public struct" +++ catToSwiftType (TokenCat tokenName) +++ "{"
        , nest 2 $ text $ "public let value: " ++ value
        , empty
        , nest 2 $ text $ "public init(_ value:" +++ value ++ ") {"
        , nest 4 $ text "self.value = value"
        , nest 2 $ text "}"
        , text "}"
        ]
      where
        value
          | tokenName == catInteger = "Int"
          | tokenName == catDouble  = "Double"
          | otherwise = "String"

    -- | Generates enums and cases for a given data type.
    prData :: Data -> [Doc]
    prData (cat, rules) = categoryClass
      where
        funs = map fst rules
        cases = mapMaybe (prRule cat) rules
        categoryClass
          | catToStr cat `elem` funs || isList cat = []
          | otherwise =
              let name = catToSwiftType cat
              in [ vcat $ text ("public indirect enum" +++ wrapIfNeeded name +++ "{")
                    : nest 2 (vcat cases)
                    : [text "}"]
                ]

    -- | Generates individual cases for enum definitions.
    prRule :: Cat -> (Fun, [Cat]) -> Maybe Doc
    prRule cat (fun, cats)
      | isNilFun fun || isOneFun fun || isConsFun fun = Nothing
      | otherwise = Just $ text $ "case" +++ caseName ++ resultAssociatedValuesConcatenated
      where
        caseName = str2SwiftClassName langName fun
        caseAssociatedValues = map (wrapIfNeeded . catToSwiftType) cats
        resultAssociatedValuesConcatenated
          | null cats = ""
          | otherwise = "(" ++ intercalate ", " caseAssociatedValues ++ ")"
