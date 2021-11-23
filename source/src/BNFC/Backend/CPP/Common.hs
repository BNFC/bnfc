{-# LANGUAGE LambdaCase #-}

-- | Common to the C++ backends.

module BNFC.Backend.CPP.Common where

import Data.Char  ( isUpper )
import Data.List  ( intercalate )

import BNFC.CF
import BNFC.TypeChecker
import BNFC.Options            ( Ansi )
import BNFC.Backend.C          ( comment )
import BNFC.Backend.CPP.Naming

-- | C++ line comment including mode hint for emacs.

commentWithEmacsModeHint :: String -> String
commentWithEmacsModeHint = comment . ("-*- c++ -*- " ++)

-- | C++ code for the @define@d constructors.
--
-- @definedRules Nothing@ only prints the header.
definedRules :: Maybe ListConstructors -> CF -> String -> String
definedRules mlc cf banner
  | null theLines = []
  | otherwise     = unlines $ banner : "" : theLines
  where
    theLines = map rule $ definitions cf

    ctx = buildContext cf

    rule (Define f args e t) =
      case mlc of
        Nothing -> header ++ ";"
        Just lc -> unlines
          [ header ++ " {"
          , "  return " ++ cppExp lc (map fst args) e ++ ";"
          , "}"
          ]
      where
        header = cppType t ++ " " ++ sanitizeCpp (funName f) ++ "(" ++
                  intercalate ", " (map cppArg args) ++ ")"

        cppType :: Base -> String
        cppType (ListT (BaseT x)) = "List" ++ x ++ "*"
        cppType (ListT t)         = cppType t ++ "*"
        cppType (BaseT x)
            | x `elem` baseTokenCatNames = x
            | isToken x ctx = "String"
            | otherwise     = x ++ "*"

        cppArg :: (String, Base) -> String
        cppArg (x,t) = cppType t ++ " " ++ x ++ "_"

        cppExp :: ListConstructors -> [String] -> Exp -> String
        cppExp (LC nil cons) args = loop
          where
          loop = \case
            App "[]"  (FunT [] (ListT t)) [] -> fst $ nil t
            App "(:)" (FunT _  (ListT t)) es -> call (fst $ cons t) es
            Var x          -> x ++ "_"  -- argument
            App t _ [e]
              | isToken t ctx    -> loop e
            App x _ es
              | isUpper (head x) -> call ("new " ++ x) es
              | x `elem` args    -> call (x ++ "_") es
              | otherwise        -> call (sanitizeCpp x) es
            LitInt n       -> show n
            LitDouble x    -> show x
            LitChar c      -> show c
            LitString s    -> show s

          call x es = x ++ "(" ++ intercalate ", " (map loop es) ++ ")"

data CppStdMode
  = CppStdAnsi Ansi -- ^ @Ansi@ mode.
  | CppStdBeyondAnsi Ansi -- ^ @BeyondAnsi@ mode.

wrapPointerIf :: Bool -> String -> String
wrapPointerIf b v = if b then "*" ++ v else v

wrapUniquePtrIf :: Bool -> String -> String
wrapUniquePtrIf b v = if b then "std::unique_ptr<" ++v++">" else v

wrapUniquePtr :: String -> String
wrapUniquePtr v = "std::unique_ptr<" ++v++">"

wrapMoveIf :: Bool -> String -> String
wrapMoveIf b v = if b then "std::move(" ++v++")" else v
