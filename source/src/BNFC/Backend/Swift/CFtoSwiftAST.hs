{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Swift.CFtoSwiftAST (cf2SwiftAST) where

import Data.Maybe      ( mapMaybe )

import BNFC.CF
import BNFC.Utils       ( (+++) )

import BNFC.Backend.Common.NamedVariables ( UserDef )
import BNFC.Backend.Swift.Common 
import Data.List (intercalate)

-- Produces abstract data types in Swift
cf2SwiftAST :: String -> CF -> String
cf2SwiftAST langName cf =  unlines
    $ imports ++ [""]-- import some libraries if needed
    -- ++ characterTypedef
    ++ map mkTokenDecl allTokenNames
    ++ concatMap prData rules  -- generate user-defined types
  where
    rules  = getAbstractSyntax cf
    imports = [ 
      "import Foundation"
      ]
    censorName' = censorName langName
    str2SwiftClassName' = str2SwiftClassName langName
    str2SwiftCaseName' = str2SwiftCaseName langName
    cat2SwiftClassName' = cat2SwiftClassName langName
    getVars' = getVars_ langName
    allTokenNames = literals cf
    
    -- | valueType is a string which represents Swift basic type.
    mkTokenDecl :: String -> String
    mkTokenDecl tokenName = unlines
        [ "public struct" +++ catToSwiftType (TokenCat tokenName) +++ "{"
        , indentStr 2 $ "let value: " ++ value
        , ""
        , indentStr 2 $ "public init(_ value:" +++ value ++ ") {"
        , indentStr 4 $ "self.value = value" 
        , indentStr 2 $ "}" 
        , "}"
        ]
      where
        value
          | tokenName == catInteger = "Int"
          | tokenName == catDouble  = "Double"
          | otherwise = "String"
          

    -- | Generates a category class, and classes for all its rules.
    prData :: Data -> [String]
    prData (cat, rules) = categoryClass
        where
        funs = map fst rules
        cases = mapMaybe (prRule cat) rules
        categoryClass
          | catToStr cat `elem` funs || isList cat = [] -- the category is also a function or a list
          | otherwise =
            let name = catToSwiftType cat
            -- let name = cat2SwiftClassName' cat // TODO: refactor, merge functions
            in 
              [ "public indirect enum" +++ wrapIfNeeded name +++ "{"
              ] ++ indent_ 1 cases ++ ["}\n"]


    -- | Generates classes for a rule, depending on what type of rule it is.
    prRule :: Cat -> (Fun, [Cat]) -> Maybe (String)
    prRule cat (fun, cats)
      | isNilFun fun || 
        isOneFun fun || 
        isConsFun fun = Nothing  -- these are not represented in the Absyn
      | otherwise = -- a standard rule
         Just result
      where
        caseName = str2SwiftClassName' fun
        vars = getVars' cats
        -- caseAssociatedValues = map (\var -> buildVariableName var ++ ": " ++ buildVariableType var) vars
        caseAssociatedValues = map (\var -> wrapIfNeeded $ catToSwiftType var) cats
        resultAssociatedValuesConcatenated
          | null vars = ""
          | otherwise = "(" ++ (intercalate ", " caseAssociatedValues) ++ ")"
        result = unwords $ ["case", caseName ++ resultAssociatedValuesConcatenated]