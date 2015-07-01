module BNFC.Backend.Common.Makefile where

import BNFC.Options (SharedOptions(..))
import BNFC.Backend.Base (mkfile, Backend)
import BNFC.PrettyPrint

-- | Creates a Makefile rule
-- >>> mkRule "main" ["file1","file2"] ["do something"]
-- main: file1 file2
-- 	do something
-- <BLANKLINE>
--
-- >>> mkRule "main" ["program.exe"] []
-- main: program.exe
-- <BLANKLINE>
mkRule :: String   -- ^ The target name
       -> [String] -- ^ Dependencies
       -> [String] -- ^ Recipe
       -> Doc
mkRule target deps recipes =
    text target <> ":" <+> hsep (map text deps)
    $$ vcat [ "\t" <> text recipe | recipe <- recipes ]
    $$ ""

-- | Variable assignment
--
-- >>> mkVar "FOO" "bar"
-- FOO=bar
mkVar :: String -> String -> Doc
mkVar n v = text n <> "=" <> text v


-- | Create the Makefile file using the name specified in the option
-- record.
mkMakefile :: SharedOptions -> Doc -> Backend
mkMakefile Options {make = Nothing} _ = return ()
mkMakefile Options {make = Just makefile} content = mkfile makefile content
