{-# LANGUAGE LambdaCase #-}

-- | Common to the C++ backends.

module BNFC.Backend.CPP.Common where

import Data.Char  ( isUpper )
import Data.List  ( nub, intercalate )

import BNFC.CF
import BNFC.TypeChecker

-- | C++ code for the @define@d constructors.

definedRules :: Bool -> CF -> String -> String
definedRules onlyHeader cf banner
  | null theLines = []
  | otherwise     = unlines $ banner : "" : theLines
  where
    theLines = map rule $ definitions cf

    ctx = buildContext cf

    list = LC
      { nil  =  const ("[]", dummyType)
      , cons = \ t -> ("List" ++ unBase t, dummyType)
      }
      where
        unBase (ListT t) = unBase t
        unBase (BaseT x) = norm x

    norm = catToStr . normCat . strToCat

    rule (Define f args e t) =
      case runTypeChecker $ checkDefinition' list ctx f xs e of
        Left err -> error $ "Panic! This should have been caught already:\n" ++ err
        Right (args,(e',t))
          | onlyHeader -> header ++ ";"
          | otherwise  -> unlines
            [ header ++ " {"
            , "  return " ++ cppExp (map fst args) e' ++ ";"
            , "}"
            ]
         where
         header = cppType t ++ " " ++ funName f ++ "(" ++
                  intercalate ", " (map cppArg args) ++ ")"
      where
        xs = map fst args
        cppType :: Base -> String
        cppType (ListT (BaseT x)) = "List" ++ norm x ++ "*"
        cppType (ListT t)         = cppType t ++ "*"
        cppType (BaseT x)
            | x `elem` baseTokenCatNames = x
            | isToken x ctx = "String"
            | otherwise     = norm x ++ "*"

        cppArg :: (String, Base) -> String
        cppArg (x,t) = cppType t ++ " " ++ x ++ "_"

        cppExp :: [String] -> Exp -> String
        cppExp args = \case
            App "[]" _ []    -> "0"
            Var x          -> x ++ "_"  -- argument
            App t _ [e]
              | isToken t ctx    -> cppExp args e
            App x _ es
              | isUpper (head x) -> call ("new " ++ x) es
              | x `elem` args    -> call (x ++ "_") es
              | otherwise        -> call x es
            LitInt n       -> show n
            LitDouble x    -> show x
            LitChar c      -> show c
            LitString s    -> show s
          where
          call x es = x ++ "(" ++ intercalate ", " (map (cppExp args) es) ++ ")"
