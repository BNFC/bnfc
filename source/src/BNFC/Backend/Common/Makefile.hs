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
  [ printf "%s: %s" target (unwords deps) ]
  ++ map (printf "\t%s") recipe
  ++ [""]

mkDoc :: String -> Makefile
mkDoc texfile = mkRule "doc" [pdffile] []
              . mkRule pdffile [texfile]
                  [ printf "pdflatex %s" texfile ]
  where pdffile = replaceExtension texfile "pdf"

--                [
-- 		 "all:",
--                 "\thappy -gca " ++ glr_params ++ happyFile opts,
--		 "\talex -g "  ++ alexFile opts,
--		 "\t" ++ cd ("latex " ++ basename (latexFile opts)
--			     ++ "; " ++ "dvips " ++ basename (dviFile opts)
--			     ++ " -o " ++ basename (psFile opts)),
--		 "\tghc --make " ++ tFile opts ++ " -o " ++ mkFile withLang "Test" "" opts,
--		 "clean:",
--		 "\t-rm -f " ++ unwords (map (dir++) [
--						       "*.log", "*.aux", "*.hi",
--						       "*.o", "*.dvi"
--						      ]),
--		 "\t-rm -f " ++ psFile opts,
--
--		 "distclean: clean",
--		 "\t-rm -f " ++ unwords [
--					 mkFile withLang "Doc" "*" opts,
--					 mkFile withLang "Lex" "*" opts,
--					 mkFile withLang "Par" "*" opts,
--					 mkFile withLang "Layout" "*" opts,
--					 mkFile withLang "Skel" "*" opts,
--					 mkFile withLang "Print" "*" opts,
--					 mkFile withLang "Test" "*" opts,
--					 mkFile withLang "Abs" "*" opts,
--					 mkFile withLang "Test" "" opts,
--					 mkFile noLang   "ErrM" "*" opts,
--					 mkFile noLang   "SharedString" "*" opts,
--                                       dir ++ lang opts ++ ".dtd",
--					 mkFile withLang "XML" "*" opts,
--					 "Makefile*"
--					],
--		 if null dir then "" else "\t-rmdir -p " ++ dir
--		]

