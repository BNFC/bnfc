-- | Functions that are used in multiple backends
module BNFC.Backend.Utils (isTokenType) where

import BNFC.CF (Cat(..))

-- | Checks if a category is a token type (either built-in or user-defined)
-- The first argument is the list of user-defined token type.
-- >>> isTokenType [] (Cat "Integer")
-- True
-- >>> isTokenType [Cat "Abc"] (Cat "Abc")
-- True
-- >>> isTokenType [] (Cat "Abc")
-- False
isTokenType :: [Cat] -> Cat -> Bool
isTokenType _ (Cat "Integer") = True
isTokenType _ (Cat "Char") = True
isTokenType _ (Cat "String") = True
isTokenType _ (Cat "Double") = True
isTokenType _ (Cat "Ident") = True
isTokenType user cat | cat `elem` user = True
isTokenType _ _ = False
