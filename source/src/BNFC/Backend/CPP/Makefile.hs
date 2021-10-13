{-# LANGUAGE OverloadedStrings #-}

module BNFC.Backend.CPP.Makefile (makefile) where

import BNFC.Backend.Common.Makefile
import BNFC.PrettyPrint

makefile :: String -> String -> String -> Doc
makefile prefix name basename = vcat
    [ mkVar "CC" "g++ -g"
    , mkVar "CCFLAGS" "--ansi -W -Wall -Wsign-conversion -Wno-unused-parameter -Wno-unused-function -Wno-unneeded-internal-declaration"
    , ""
    , mkVar "FLEX" "flex"
    , mkVar "FLEX_OPTS" ("-P" ++ prefix)
    , ""
    , mkVar "BISON" "bison"
    , mkVar "BISON_OPTS" ("-t -p" ++ prefix)
    , ""
    , mkVar "OBJS" "Absyn.o Buffer.o Lexer.o Parser.o Printer.o"
    , ""
    , mkRule ".PHONY" ["clean", "distclean"]
        []
    , mkRule "all" [testName]
        []
    , mkRule "clean" []
        -- peteg: don't nuke what we generated - move that to the "vclean" target.
        [ "rm -f *.o " ++ testName ++ " " ++ unwords
            [ name ++ e | e <- [".aux", ".log", ".pdf",".dvi", ".ps", ""]] ]
    , mkRule "distclean" ["clean"]
        [ "rm -f " ++ unwords
            [ "Absyn.C", "Absyn.H"
            , "Buffer.C", "Buffer.H"
            , "Test.C"
            , "Bison.H", "Parser.C", "Parser.H", "ParserError.H", name ++ ".y"
            , "Lexer.C", name ++ ".l"
            , "Skeleton.C", "Skeleton.H"
            , "Printer.C", "Printer.H"
            , basename
            , name ++ ".tex"
            ]
        ]
    , mkRule testName [ "${OBJS}", "Test.o" ]
        [ "@echo \"Linking " ++ testName ++ "...\""
        , "${CC} ${OBJS} Test.o -o " ++ testName ]
    , mkRule "Absyn.o" [ "Absyn.C", "Absyn.H" ]
        [ "${CC} ${CCFLAGS} -c Absyn.C" ]
    , mkRule "Buffer.o" [ "Buffer.C", "Buffer.H" ]
        [ "${CC} ${CCFLAGS} -c Buffer.C " ]
    , mkRule "Lexer.C" [ name ++ ".l" ]
        [ "${FLEX} ${FLEX_OPTS} -oLexer.C " ++ name ++ ".l" ]
    , mkRule "Parser.C Bison.H" [ name ++ ".y" ]
      [ "${BISON} ${BISON_OPTS} " ++ name ++ ".y -o Parser.C" ]
    , mkRule "Lexer.o" [ "Lexer.C", "Bison.H" ]
        [ "${CC} ${CCFLAGS} -c Lexer.C " ]
    , mkRule "Parser.o" [ "Parser.C", "Absyn.H", "Bison.H" ]
        [ "${CC} ${CCFLAGS} -c Parser.C" ]
    , mkRule "Printer.o" [ "Printer.C", "Printer.H", "Absyn.H" ]
        [ "${CC} ${CCFLAGS} -c Printer.C" ]
    , mkRule "Skeleton.o" [ "Skeleton.C", "Skeleton.H", "Absyn.H" ]
       [ "${CC} ${CCFLAGS} -Wno-unused-parameter -c Skeleton.C" ]
    , mkRule "Test.o" [ "Test.C", "Parser.H", "Printer.H", "Absyn.H" ]
        [ "${CC} ${CCFLAGS} -c Test.C" ]
    ]
  where testName = "Test" ++ name
