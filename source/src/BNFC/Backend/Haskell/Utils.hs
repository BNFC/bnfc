module BNFC.Backend.Haskell.Utils (parserName) where

import Text.PrettyPrint
import BNFC.CF (Cat(..), identCat)

-- | Create a valid parser function name for a given category
-- >>> parserName (Cat "Abcd")
-- pAbcd
-- >>> parserName (ListCat (Cat "Xyz"))
-- pListXyz
parserName :: Cat -> Doc
parserName = ("p" <>) . text . identCat
