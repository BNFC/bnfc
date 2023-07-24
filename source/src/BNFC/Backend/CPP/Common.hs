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
definedRules :: CppStdMode -> Maybe ListConstructors -> CF -> String -> String
definedRules mode mlc cf banner
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

        -- if ansi mode: T*
        -- if beyond ansi mode: shared_ptr<T>
        wrapSharedPtrByMode :: String -> String
        wrapSharedPtrByMode x = case mode of
                                  CppStdAnsi _       -> x ++ "*"
                                  CppStdBeyondAnsi _ -> wrapSharedPtr x
        -- ansi mode: new T
        -- beyond ansi mode: std::make_shared<T>
        wrapInstantiateByMode :: String -> String
        wrapInstantiateByMode x = case mode of
                                    CppStdAnsi _       -> "new " ++ x
                                    CppStdBeyondAnsi _ -> wrapMakeShared x

        cppType :: Base -> String
        cppType (ListT (BaseT x)) = wrapSharedPtrByMode ("List" ++ x)
        cppType (ListT t)         = wrapSharedPtrByMode (cppType t)
        cppType (BaseT x)
            | x `elem` baseTokenCatNames = x
            | isToken x ctx = "String"
            | otherwise     = wrapSharedPtrByMode x

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
              | isUpper (head x) -> call (wrapInstantiateByMode x) es
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

wrapSharedPtrIf :: Bool -> String -> String
wrapSharedPtrIf b v = if b then "std::shared_ptr<" ++v++">" else v

wrapSharedPtr :: String -> String
wrapSharedPtr v = "std::shared_ptr<" ++v++">"

wrapMakeShared :: String -> String
wrapMakeShared v = "std::make_shared<" ++v++">"
