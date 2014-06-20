module BNFC.Backend.Common.Makefile where

import Text.Printf
import Text.Show (ShowS) -- Efficient string concatenation
import System.FilePath (replaceExtension)

import BNFC.Options (SharedOptions(..))
import BNFC.Backend.Base (mkfile, Backend)

type Makefile = ShowS


mkRule :: String   -- ^ The target name
       -> [String] -- ^ Dependencies
       -> [String] -- ^ Recipe
       -> Makefile
mkRule target deps recipe = (++) $ unlines $
  [ unwords (printf "%s:" target:deps) ]
  ++ map (printf "\t%s") recipe
  ++ [""]

-- | Create the Makefile file using the name specified in the option
-- record.
mkMakefile :: SharedOptions -> String -> Backend
mkMakefile Options {make = Nothing} _ = return ()
mkMakefile Options {make = Just makefile} content = mkfile makefile content
