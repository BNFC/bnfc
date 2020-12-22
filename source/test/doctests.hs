-- | @doctest@ main, from https://hackage.haskell.org/package/cabal-doctest

import Build_doctests      ( flags, pkgs, module_sources )
import System.Environment  ( unsetEnv )
import Test.DocTest        ( doctest )

main :: IO ()
main = do
    -- optionally print arguments
    mapM_ putStrLn . concat $
      [ ["Arguments to doctest"]
      , args
      , [""]
      ]
    fixEnv
    doctest args
  where
    args = flags ++ pkgs ++ module_sources

-- | From version 2, Stack sets the GHC_ENVRIONMENT variable, and GHC
-- (as invoked by doctest) will pick that up. This is undesirable:
-- cabal-doctest passes all the necessary information on the command
-- line already, and can lead to ambiguous module errors as GHC will
-- load the environment in addition to what cabal-doctest instructs it
-- to.
--
-- Hence, cabal-doctest tells GHC to ignore package environments
-- altogether on the command line. However, this is only possible
-- since GHC 8.2. If you are using cabal-doctest with Stack 2 and
-- GHC 8.0 or earlier and seeing ambiguous module errors or other
-- mysterious failures, try manually unsetting GHC_ENVIRONMENT before
-- invoking doctest.
--
fixEnv :: IO ()
fixEnv = unsetEnv "GHC_ENVIRONMENT"
