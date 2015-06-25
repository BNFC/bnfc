import Test.DocTest
main = doctest
    [ "-isrc"
    , "-idist/build/autogen/"
    , "-idist/build/bnfc/bnfc-tmp"
    , "-XOverloadedStrings"
    , "-XRecordWildCards"
    , "src/PrintBNF.hs"
    , "src/Main.hs" ]
