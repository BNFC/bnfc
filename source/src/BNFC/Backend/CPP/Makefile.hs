{-# LANGUAGE OverloadedStrings #-}

module BNFC.Backend.CPP.Makefile (makefile) where
import BNFC.Options
import BNFC.Backend.Common.Makefile
import BNFC.PrettyPrint
import BNFC.Utils                (when)

makefile :: String -> String -> SharedOptions -> String -> Doc
makefile prefix name opts basename =
  vcat $
    [ mkVar "CC" "g++ -g"
    , mkVar "CCFLAGS" (compileOpt ++ " -W -Wall -Wsign-conversion -Wno-unused-parameter -Wno-unused-function -Wno-unneeded-internal-declaration")
    , ""
    , mkVar "FLEX" "flex"
    , mkVar "FLEX_OPTS" ("-P" ++ prefix)
    , ""
    , mkVar "BISON" "bison"
    , mkVar "BISON_OPTS" ("-t -p" ++ prefix)
    , ""
    , if (ansi opts /= Ansi) then
        mkVar "OBJS" "Absyn.o Buffer.o Lexer.o Parser.o Driver.o Printer.o"
      else
        mkVar "OBJS" "Absyn.o Buffer.o Lexer.o Parser.o Printer.o"
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
            [ "Absyn" ++ cppExt, "Absyn" ++ hExt
            , "Buffer" ++ cppExt, "Buffer" ++ hExt
            , "Test" ++ cppExt
            , "Bison" ++ hExt, "Parser" ++ cppExt, "Parser" ++ hExt, "ParserError" ++ hExt, name ++ parserExt
            , "Lexer" ++ cppExt, name ++ lexerExt
            , "Skeleton" ++ cppExt, "Skeleton" ++ hExt
            , "Printer" ++ cppExt, "Printer" ++ hExt
            , "Driver" ++ cppExt, "Driver" ++ hExt
            , "Scanner" ++ hExt
            , "location" ++ hExt
            , basename
            , name ++ ".tex"
            ]
        ]
    , mkRule testName [ "${OBJS}", "Test.o" ]
        [ "@echo \"Linking " ++ testName ++ "...\""
        , "${CC} ${OBJS} Test.o -o " ++ testName ]
    , mkRule "Absyn.o" [ "Absyn" ++ cppExt, "Absyn" ++ hExt ]
        [ "${CC} ${CCFLAGS} -c Absyn" ++ cppExt ]
    , when (ansi opts /= Ansi)
      mkRule "Driver.o" [ "Driver" ++ cppExt, "Driver" ++ hExt ]
      [ "${CC} ${CCFLAGS} -c Driver" ++ cppExt ]
    , mkRule "Buffer.o" [ "Buffer" ++ cppExt, "Buffer" ++ hExt ]
        [ "${CC} ${CCFLAGS} -c Buffer" ++ cppExt ]
    , mkRule ("Lexer" ++ cppExt) [ name ++ lexerExt ]
        [ "${FLEX} ${FLEX_OPTS} -oLexer" ++ cppExt ++ " " ++ name ++ lexerExt ]
    , mkRule ("Parser"  ++ cppExt++ " Bison" ++ hExt) [ name ++ parserExt ]
      [ "${BISON} ${BISON_OPTS} " ++ name ++ parserExt ++ " -o Parser" ++ cppExt ]
    , mkRule "Lexer.o" [ "CCFLAGS+=-Wno-sign-conversion" ]
        []
    , mkRule "Lexer.o" [ "Lexer" ++ cppExt, "Bison" ++ hExt ]
        [ "${CC} ${CCFLAGS} -c Lexer" ++ cppExt ]
    , mkRule "Parser.o" [ "Parser" ++ cppExt, "Absyn" ++ hExt, "Bison" ++ hExt ]
        [ "${CC} ${CCFLAGS} -c Parser" ++ cppExt ]
    , mkRule "Printer.o" [ "Printer" ++ cppExt, "Printer" ++ hExt, "Absyn" ++ hExt ]
        [ "${CC} ${CCFLAGS} -c Printer" ++ cppExt ]
    , mkRule "Skeleton.o" [ "Skeleton" ++ cppExt, "Skeleton" ++ hExt, "Absyn" ++ hExt ]
       [ "${CC} ${CCFLAGS} -Wno-unused-parameter -c Skeleton" ++ cppExt ]
    , mkRule "Test.o" [ "Test" ++ cppExt, "Parser" ++ hExt, "Printer" ++ hExt, "Absyn" ++ hExt ]
        [ "${CC} ${CCFLAGS} -c Test" ++ cppExt ]
    ]
  where
    testName = "Test" ++ name
    compileOpt = if Ansi == ansi opts then "--ansi" else "-std=c++14"
    lexerExt = if Ansi == ansi opts then ".l" else ".ll"
    parserExt = if Ansi == ansi opts then ".y" else ".yy"
    cppExt = if Ansi == ansi opts then ".c" else ".cc"
    hExt = if Ansi == ansi opts then ".h" else ".hh"
