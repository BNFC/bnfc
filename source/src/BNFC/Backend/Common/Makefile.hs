module BNFC.Backend.Common.Makefile where

import Text.Printf
import Text.Show (ShowS) -- Efficient string concatenation
import System.FilePath (replaceExtension)

type Makefile = ShowS


mkRule :: String   -- ^ The target name
       -> [String] -- ^ Dependencies
       -> [String] -- ^ Recipe
       -> Makefile
mkRule target deps recipe = (++) $ unlines $
  [ unwords (printf "%s:" target:deps) ]
  ++ map (printf "\t%s") recipe
  ++ [""]

mkDoc :: String -> Makefile
mkDoc texfile = mkRule "doc" [pdffile] []
              . mkRule pdffile [texfile]
                  [ printf "pdflatex %s" texfile ]
  where pdffile = replaceExtension texfile "pdf"
