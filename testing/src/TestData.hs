{-# LANGUAGE DeriveFunctor #-}

module TestData (exampleGrammars, Example, Example'(..)) where

import Shelly ((</>), FilePath)
import Prelude hiding (FilePath)

data Example' a = Example
  { grammarFile   :: a
      -- ^ Name of the LBNF grammar file.
  , exampleInputs :: [a]
      -- ^ Files to test the generated parser with.
  }
  deriving (Functor)

type Example = Example' FilePath

-- The data to test the different backends with. The first file should be
-- a lbnf grammar and the list contains example programs written in this
-- languague. The list can contain zero, one or more example files. If there
-- is zero, we only test that the grammar is correctly compiled. If there is
-- ore or more, they are fed to the test program and we expect that it exits
-- successfully (i.e. exit code 0).
exampleGrammars :: [Example]
exampleGrammars = map (fmap prefix) $
  [ fmap ("cpp"       </>) $ Example "cpp.cf"    [ "example.cpp" ]
  , fmap ("GF"        </>) $ Example "gf.cf"     [ "example.gf"  ]
  , fmap ("OCL"       </>) $ Example "OCL.cf"    [ "example.ocl" ]
  , fmap ("prolog"    </>) $ Example "Prolog.cf" [ "small.pl", "simpsons.pl" ]
  , fmap ("C"         </>) $ Example "C.cf"      [ "runtime.c", "koe2.c" ]
  , fmap ("C"         </>) $ Example "C4.cf"     [ "koe2.c" ]
  , fmap ("C"         </>) $ Example "C_with_delimiters.cf" [ "small.c" ]  -- "core.c" fails with CNF!!!
  , fmap ("Javalette" </>) $ Example "JavaletteLight.cf"    [ "koe.jll" ]
  , fmap ("LBNF"      </>) $ Example "LBNF.cf"   [ "LBNF.cf" ]
  , fmap ("Java"      </>) $ Example "java.cf"   []
  , Example "Calc.cf" []
  , Example "fstStudio.cf" []
  ]
  where
  prefix file = ".." </> "examples" </> file
