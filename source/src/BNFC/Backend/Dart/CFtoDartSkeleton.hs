{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Dart.CFtoDartSkeleton (cf2DartSkeleton) where

import Data.Maybe      ( mapMaybe )

import BNFC.CF
import BNFC.Utils       ( (+++) )

import BNFC.Backend.Common.NamedVariables ( UserDef )
import BNFC.Backend.Dart.Common 


cf2DartSkeleton :: String -> CF -> String -> String
cf2DartSkeleton langName cf importLang = 
  unlines $ 
    [ "import 'package:fast_immutable_collections/fast_immutable_collections.dart';"
    , importLang
    , "A identityFn<A>(A a) => a;" ]
    ++ (map buildUserToken [ n | (n,_) <- tokenPragmas cf ]) -- generate user-defined types
    ++ (concatMap genData $ getAbstractSyntax cf)
  where
    censorName' = censorName langName
    str2DartClassName' = str2DartClassName langName
    getVars' = getVars langName
    cat2DartClassName' = cat2DartClassName langName
    cat2DartType' = cat2DartType langName
    buildUserToken :: UserDef -> String
    buildUserToken token = 
      "String interpret" ++ (censorName' token) ++ "(x) => x;" 

    genData :: Data -> [String]
    genData (cat, rules)
      | (catToStr cat) `elem` (map fst rules) = [] -- the category is also a function 
      | otherwise = 
        let name = cat2DartClassName' cat
            varType = buildVariableTypeFromDartType $ cat2DartType' cat
        in [ "String interpret" ++ name ++ "(" ++ varType +++ "e) =>" ] 
          ++ (indent 1 $ if isList cat 
              then [ "\"$e\";" ] 
              else [ "switch (e) {" ]
                  ++ (indent 1 $ mapMaybe genBranch rules)
                  ++ [ "};" ])

    genBranch :: (Fun, [Cat]) -> Maybe (String)
    genBranch (fun, rhs) 
      | isNilFun fun || 
        isOneFun fun || 
        isConsFun fun = Nothing  -- these are not represented in the Absyn
      | otherwise = -- a standard rule
        let 
          className = str2DartClassName' fun
          varName = lowerFirst $ censorName' className
          vars = getVars' rhs
        in Just $ 
          className +++ varName +++ "=> \"" ++ className ++ "("
          ++ (concat $ (drop 1) $ arguments (genVarRepr varName) vars)
          ++ ")\","
      where 
        arguments _ [] = []
        arguments generator (x:vars) = 
          [ ", ", "${" ++ (generator x) ++ "}" ] ++ (arguments generator vars)

    genVarRepr :: String -> DartVar -> String
    genVarRepr varName variable@((n, varType), _) = let 
      varCall = varName ++ "." ++ (buildVariableName variable)
      interp = interpreter varType in
        if n > 0 then 
          varCall ++ ".map(" ++ (unpack interp (n - 1))  ++ ")"
        else 
          interp ++ "(" ++ varCall ++ ")"
      where 
        unpack funName n 
          | n <= 0 = funName
          | otherwise = let varName = "e" ++ show n in
            "(" ++ varName ++ ") => " ++ varName ++ ".map(" ++ (unpack funName (n - 1)) ++ ")"
        interpreter varType 
          | varType /= (censorName' varType) = "identityFn"
          | otherwise = "interpret" ++ varType
