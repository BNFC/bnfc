module BNFC.Backend.Common.Makefile where

import Text.Printf

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

mkVar :: String -> String -> Makefile
mkVar n v = (++) (n ++ "=" ++ v  ++ "\n")


-- | Create the Makefile file using the name specified in the option
-- record.
mkMakefile :: SharedOptions -> String -> Backend
mkMakefile Options {make = Nothing} _ = return ()
mkMakefile Options {make = Just makefile} content = mkfile makefile content
