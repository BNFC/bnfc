{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Swift.CFtoSwiftSkeleton (cf2SwiftSkeleton) where

import Data.Maybe      ( mapMaybe )

import BNFC.CF
import BNFC.Utils       ( (+++) )

import BNFC.Backend.Common.NamedVariables ( UserDef )
import BNFC.Backend.Swift.Common 
import Data.List (intercalate)


cf2SwiftSkeleton :: String -> CF -> String
cf2SwiftSkeleton langName cf = 
  unlines $ 
    [ "import Foundation"
    , "import" +++ langName
    , ""
    , "func identityFn<A>(_ a: A) -> A { a }"
    , "" ]
    ++ (map buildUserToken [ n | (n,_) <- tokenPragmas cf ])
    ++ (concatMap genData $ getAbstractSyntax cf)
  where
    censorName' = censorName langName
    str2SwiftClassName' = str2SwiftClassName langName
    getVars' = getVars_ langName
    cat2SwiftClassName' = cat2SwiftClassName langName
    cat2SwiftType' = cat2SwiftType langName
    buildUserToken :: UserDef -> String
    buildUserToken token = 
      "String interpret" ++ (censorName' token) ++ "(x) => x;" 

    genData :: Data -> [String]
    genData (cat, rules)
      | (catToStr cat) `elem` (map fst rules) = []
      | otherwise = 
        let name = cat2SwiftClassName' cat
            varType = buildVariableTypeFromSwiftType $ cat2SwiftType' cat
        in [ "func interpret" ++ name ++ "(_ e:" +++ varType ++ ") -> String {" ]
          ++ (indent_ 1 $ if isList cat 
              then [ "\"\\(e)\"" ] 
              else [ "switch (e) {" ]
                  ++ (indent_ 1 $ mapMaybe genBranch rules)
                  ++ [ "}" ])
          ++ ["}"]
          ++ [""]

    genBranch :: (Fun, [Cat]) -> Maybe (String)
    genBranch (fun, rhs) 
      | isNilFun fun || 
        isOneFun fun || 
        isConsFun fun = Nothing
      | otherwise =
        let 
          className = str2SwiftClassName' fun
          varName = lowerFirst $ censorName' className
          vars = getVars' rhs
        in Just $ 
          "case let ." ++ className ++ "(" ++ (intercalate ", " (associatedValues vars)) ++ "): \"" ++ className ++ "("
          ++ (concat $ (drop 1) $ arguments (genVarRepr varName) vars)
          ++ ")\""
      where
        associatedValues [] = []
        associatedValues (x: vars) = [buildVariableName x] ++ (associatedValues vars)

        arguments _ [] = []
        arguments generator (x:vars) = 
          [ ", ", "\\(" ++ (generator x) ++ ")" ] ++ (arguments generator vars)

    genVarRepr :: String -> SwiftVar -> String
    genVarRepr varName variable@((n, varType), _) = let 
      varCall = buildVariableName variable
      interp = interpreter varType in
        if n > 0 then 
          varCall ++ ".map(" ++ (unpack interp (n - 1))  ++ ")" -- TODO: check this
        else 
          interp ++ "(" ++ varCall ++ ")"
      where 
        unpack funName n 
          | n <= 0 = funName
          | otherwise = let varName = "e" ++ show n in
            "{ " ++ varName ++ " in " ++ varName ++ ".map { " ++ (unpack funName (n - 1)) ++ " } }" -- TODO: check this
        interpreter varType 
          | varType /= (censorName' varType) = "identityFn"
          | otherwise = "interpret" ++ varType
