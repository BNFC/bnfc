import Test.DocTest
main = doctest
    [ "-isrc"
    , "-idist/build/autogen/"
    , "-idist/build/bnfc/bnfc-tmp"
    , "-XOverloadedStrings"
    , "src/Main.hs" ]
