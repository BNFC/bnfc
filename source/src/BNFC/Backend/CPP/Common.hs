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
    theLines = [ rule f xs e | FunDef f xs e <- cfgPragmas cf ]

    ctx = buildContext cf

    list = LC (const "[]") (\ t -> "List" ++ unBase t)
      where
        unBase (ListT t) = unBase t
        unBase (BaseT x) = show $ normCat $ strToCat x

    rule f xs e =
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
        cppType :: Base -> String
        cppType (ListT (BaseT x)) = "List" ++ show (normCat $ strToCat x) ++ "*"
        cppType (ListT t)         = cppType t ++ "*"
        cppType (BaseT x)
            | x `elem` baseTokenCatNames = x
            | isToken x ctx = "String"
            | otherwise     = show (normCat $ strToCat x) ++ "*"

        cppArg :: (String, Base) -> String
        cppArg (x,t) = cppType t ++ " " ++ x ++ "_"

        cppExp :: [String] -> Exp -> String
        cppExp args = \case
            App "[]" []    -> "0"
            Var x          -> x ++ "_"  -- argument
            App t [e]
              | isToken t ctx    -> cppExp args e
            App x es
              | isUpper (head x) -> call ("new " ++ x) es
              | x `elem` args    -> call (x ++ "_") es
              | otherwise        -> call x es
            LitInt n       -> show n
            LitDouble x    -> show x
            LitChar c      -> show c
            LitString s    -> show s
          where
          call x es = x ++ "(" ++ intercalate ", " (map (cppExp args) es) ++ ")"
