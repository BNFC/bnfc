{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BNFC.Backend.Dart.CFtoDartSkeleton (cf2DartSkeleton) where

import Data.Maybe      ( mapMaybe )

import BNFC.CF
import BNFC.Utils       ( (+++) )

import BNFC.Backend.Common.NamedVariables ( UserDef )
import BNFC.Backend.Dart.Common 


cf2DartSkeleton :: CF -> String -> String
cf2DartSkeleton cf importLang = 
  unlines $ 
    [ importLang
    , "A identityFn<A>(A a) => a;" ]
    ++ (map buildUserToken [ n | (n,_) <- tokenPragmas cf ]) -- generate user-defined types
    ++ (concatMap genData $ getAbstractSyntax cf)

buildUserToken :: UserDef -> String
buildUserToken token = 
  "String interpret" ++ (censorName token) ++ "(x) => x.value;" 

genData :: Data -> [String]
genData (cat, rules)
  | (catToStr cat) `elem` (map fst rules) || isList cat = [] -- the category is also a function or a list
  | otherwise = 
    let name = cat2DartClassName cat
    in [ "String interpret" ++ name ++ "(" ++ name +++ "e) => switch (e) {" ]
        ++ (indent 1 $ mapMaybe genBranch rules)
        ++ [ "};" ]

genBranch :: (Fun, [Cat]) -> Maybe (String)
genBranch (fun, rhs) 
  | isNilFun fun || 
    isOneFun fun || 
    isConsFun fun = Nothing  -- these are not represented in the Absyn
  | otherwise = -- a standard rule
    let 
      className = str2DartClassName fun
      varName = lowerFirst $ censorName className
      vars = getVars rhs
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
      | varType /= (censorName varType) = "identityFn"
      | otherwise = "interpret" ++ varType
