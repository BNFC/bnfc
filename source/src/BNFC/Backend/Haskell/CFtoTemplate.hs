{-
    BNF Converter: Template Generator
    Copyright (C) 2004  Author:  Markus Forsberg

-}

{-# LANGUAGE OverloadedStrings #-}

module BNFC.Backend.Haskell.CFtoTemplate (cf2Template) where

import Prelude hiding ((<>))

import BNFC.CF
import BNFC.PrettyPrint
import BNFC.Utils                 ( ModuleName )
import BNFC.Backend.Haskell.Utils ( catvars, languageSafe, noWarnUnusedMatches )

cf2Template :: ModuleName -> ModuleName -> Bool -> CF -> String
cf2Template skelName absName functor cf = unlines $ concat
  [ [ "-- Templates for pattern matching on abstract syntax"
    , ""
    , noWarnUnusedMatches
    , ""
    ]
  , languageSafe
  , [ ""
    , "module "++ skelName ++ " where"
    , ""
    , "import Prelude (($), Either(..), String, (++), Show, show)"
    ]
  , [ "import qualified " ++ absName | importAbsMod ]
  , [ ""
    , "type Err = Either String"
    , "type Result = Err String"
    , ""
    , "failure :: Show a => a -> Result"
    , "failure x = Left $ \"Undefined case: \" ++ show x"
    , ""
    , render . vsep $ map (uncurry (case_fun absName functor)) datas
    ]
  ]
  where
  datas        = specialData cf ++ cf2data cf
  importAbsMod = not $ null datas


-- |
-- >>> case_fun "M" False (Cat "Expr") [("EInt", [TokenCat "Integer"]), ("EAdd", [Cat "Expr", Cat "Expr"])]
-- transExpr :: M.Expr -> Result
-- transExpr x = case x of
--   M.EInt integer -> failure x
--   M.EAdd expr1 expr2 -> failure x
--
-- >>> case_fun "" True (Cat "Expr") [("EInt", [TokenCat "Integer"]), ("EAdd", [Cat "Expr", Cat "Expr"])]
-- transExpr :: Show a => Expr' a -> Result
-- transExpr x = case x of
--   EInt _ integer -> failure x
--   EAdd _ expr1 expr2 -> failure x
--
-- TokenCat are not generated as functors:
-- >>> case_fun "" True (TokenCat "MyIdent") [("MyIdent", [TokenCat "String"])]
-- transMyIdent :: MyIdent -> Result
-- transMyIdent x = case x of
--   MyIdent string -> failure x
case_fun :: ModuleName -> Bool -> Cat -> [(Fun,[Cat])] -> Doc
case_fun absName functor' cat xs = vcat
    [ fname <+> "::" <+> iffunctor "Show a =>" <+> type_ <+> "-> Result"
    , fname <+> "x = case x of"
    , nest 2 $ vcat (map mkOne xs)
    ]
  where
    -- If the functor option is set AND the category is not a token type,
    -- then the type is a functor.
    iffunctor doc | functor' && not (isTokenCat cat) = doc
                  | otherwise = empty
    type_ = qualify $ cat' <> iffunctor "' a"
    fname = "trans" <> cat'
    cat' = pretty cat
    mkOne (cons, args) =
        let ns = catvars [render fname] args -- names False (map (checkRes .var) args) 1
        in  qualify (text cons) <+> iffunctor "_" <+> hsep ns <+> "-> failure x"
    qualify :: Doc -> Doc
    qualify
      | null absName = id
      | otherwise    = (text absName <> "." <>)
