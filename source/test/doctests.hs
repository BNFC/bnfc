import System.FilePath ( (</>), joinPath )
import Test.DocTest    ( doctest )

main = doctest $ concat
  [ map ("-i" ++)
    [ "src"
    , joinPath [ "dist", "build", "bnfc-doctests", "autogen" ]
    , joinPath [ "dist", "build", "bnfc", "bnfc-tmp" ]
    ]
  , map ("-X" ++)
    -- Keep the list of language extensions in sync with BNFC.cabal!
    -- Keep in alphabetical order.
    -- Exclude "CPP".
    [ "DefaultSignatures"
    , "DoAndIfThenElse"
    , "FlexibleContexts"
    , "FlexibleInstances"
    , "LambdaCase"
    , "PatternGuards"
    , "OverloadedStrings"
    , "RecordWildCards"
    , "ScopedTypeVariables"
    , "TupleSections"
    ]
  , map ("src" </>)
    [ "PrintBNF.hs"
    , "Main.hs"
    ]
  ]
