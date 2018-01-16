import Test.DocTest

main = doctest
    [ "-isrc"
    , "-idist/build/autogen/"
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
