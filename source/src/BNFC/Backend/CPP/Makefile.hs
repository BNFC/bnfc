module BNFC.Backend.CPP.Makefile (makefile) where

import BNFC.Backend.Common.Makefile
import BNFC.PrettyPrint

makefile :: String -> Doc
makefile name = vcat
    [ mkVar "CC" "g++"
    , mkVar "CCFLAGS" "-g -W -Wall"
    , ""
    , mkVar "FLEX" "flex"
    , mkVar "FLEX_OPTS" ("-P" ++ name)
    , ""
    , mkVar "BISON" "bison"
    , mkVar "BISON_OPTS" ("-t -p" ++ name)
    , ""
    , mkVar "OBJS" "Absyn.o Lexer.o Parser.o Printer.o"
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
            [ "Absyn.C", "Absyn.H", "Test.C", "Parser.C", "Parser.H", "Lexer.C",
              "Skeleton.C", "Skeleton.H", "Printer.C", "Printer.H", "Makefile " ]
            ++ name ++ ".l " ++ name ++ ".y " ++ name ++ ".tex "]
    , mkRule testName [ "${OBJS}", "Test.o" ]
        [ "@echo \"Linking " ++ testName ++ "...\""
        , "${CC} ${CCFLAGS} ${OBJS} Test.o -o " ++ testName ]
    , mkRule "Absyn.o" [ "Absyn.C", "Absyn.H" ]
        [ "${CC} ${CCFLAGS} -c Absyn.C" ]
    , mkRule "Lexer.C" [ name ++ ".l" ]
        [ "${FLEX} -oLexer.C " ++ name ++ ".l" ]
    , mkRule "Parser.C" [ name ++ ".y" ]
      [ "${BISON} " ++ name ++ ".y -o Parser.C" ]
    , mkRule "Lexer.o" [ "Lexer.C", "Parser.H" ]
        [ "${CC} ${CCFLAGS} -c Lexer.C " ]
    , mkRule "Parser.o" [ "Parser.C", "Absyn.H" ]
        [ "${CC} ${CCFLAGS} -c Parser.C" ]
    , mkRule "Printer.o" [ "Printer.C", "Printer.H", "Absyn.H" ]
        [ "${CC} ${CCFLAGS} -c Printer.C" ]
    , mkRule "Skeleton.o" [ "Skeleton.C", "Skeleton.H", "Absyn.H" ]
       [ "${CC} ${CCFLAGS} -Wno-unused-parameter -c Skeleton.C" ]
    , mkRule "Test.o" [ "Test.C", "Parser.H", "Printer.H", "Absyn.H" ]
        [ "${CC} ${CCFLAGS} -c Test.C" ]
    ]
  where testName = "Test" ++ name
