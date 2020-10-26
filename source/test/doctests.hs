import Test.DocTest

main = doctest
    [ "-icompat"
    , "-isrc"
    , "-idist/build/bnfc-doctests/autogen/"
    , "-idist/build/bnfc/bnfc-tmp"
    , "-XLambdaCase"
    , "-XFlexibleContexts"
    , "-XOverloadedStrings"
    , "-XRecordWildCards"
    , "-XScopedTypeVariables"
    , "-XTupleSections"
    , "src/PrintBNF.hs"
    , "src/Main.hs"
    ]
