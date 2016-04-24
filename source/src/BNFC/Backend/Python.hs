{-
    BNF Converter: Java Top File
    Copyright (C) 2004  Author:  Markus Forsberg, Peter Gammie,
                                 Michael Pellauer, Bjorn Bringert

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

-------------------------------------------------------------------
-- |
-- Module      :  JavaTop
-- Copyright   :  (C)opyright 2003, {markus, aarne, pellauer, peteg, bringert} at cs dot chalmers dot se
-- License     :  GPL (see COPYING for details)
--
-- Maintainer  :  {markus, aarne} at cs dot chalmers dot se
-- Stability   :  alpha
-- Portability :  Haskell98
--
-- Top-level for the Java back end.
--
-- > $Id: JavaTop15.hs,v 1.12 2007/01/08 18:20:23 aarne Exp $
-------------------------------------------------------------------

module BNFC.Backend.Python ( makePython ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------
import BNFC.Utils
import BNFC.CF
import BNFC.Options as Options
import BNFC.Backend.Base
import BNFC.Backend.Python.AbsPython
import BNFC.Backend.Python.Utils
import BNFC.Backend.Common.Antlr4.CFtoAntlr4Lexer
import BNFC.Backend.Common.Antlr4.CFtoAntlr4Parser
import BNFC.Backend.Python.CFtoPyAbsyn
import BNFC.Backend.Python.CFtoPyPrinter
import BNFC.Backend.Python.CFtoPyVisitSkel
import BNFC.Backend.Common.NamedVariables (firstLowerCase)
import BNFC.Backend.Common.MultipleParserGenerationTools
import BNFC.Backend.Python.AntlrAdapter
import BNFC.Backend.Common.Makefile
import BNFC.PrettyPrint
import System.FilePath (pathSeparator)
import Data.Char (toLower)
-------------------------------------------------------------------
-- | Build the Java output.
-------------------------------------------------------------------



makePython :: SharedOptions -> CF -> MkFiles ()
makePython options@Options{..} cf = 
    do -- Create the package directories if necessary.
      let 
          pyex str = dirBase ++ str +.+ "py"
          bnfcfiles = bnfcVisitorsAndTests packBase cf
                            cf2PyPrinter
                            cf2PyVisitSkel
                            (cf2PyAbsyn linenumbers)
                            (testclass parselexspec
                                (head $ results lexmake) -- lexer class
                                (head $ results parmake) -- parser class
                                )
          makebnfcfile x = mkfile (pyex (fst $ x bnfcfiles))
                                            (snd $ x bnfcfiles)
    -- todo : write abstract syntax classes
    {-
      let absynFiles = remDups $ cf2JavaAbs linenumbers packageBase packageAbsyn cf
          absynBaseNames = map fst absynFiles
          absynFileNames = map (dirAbsyn ++) absynBaseNames
      let writeAbsyn (filename, contents) =
               mkfile (dirAbsyn ++ filename ++ ".java") contents
      mapM_ writeAbsyn absynFiles
      -}
      makebnfcfile babsyn
      makebnfcfile bprettyprinter      
      makebnfcfile bskel
      makebnfcfile btest
      let (lex, env) = lexfun tpar cf
      -- Where the lexer file is created. lex is the content!
      mkfile (dirBase ++ inputfile lexmake ) lex
      liftIO $ putStrLn $ "   (Tested with "+++ toolname lexmake
                                            +++ toolversion lexmake  +++")"
      -- where the parser file is created.
      mkfile (dirBase ++ inputfile parmake)
            $ parsefun tpar cf env
      liftIO $ putStrLn (if supportsEntryPoints parmake then
                              "(Parser created for all categories)" else
                              "   (Parser created only for category " ++
                                show (firstEntry cf) ++ ")")
      liftIO $ putStrLn $ "   (Tested with " +++ toolname parmake
                                             +++ toolversion parmake +++ ")"
                                             
      mkMakefile options $ makefile tpar dirBase parselexspec
    where
      packBase  = case inPackage of
                             Nothing -> lang
                             Just p -> p ++ "." ++ lang
      dirBase      = pkgToDir inPackage packBase
      pkgToDir :: Maybe String  -> String -> FilePath
      pkgToDir Nothing _ = ""
      pkgToDir _ s = replace '.' pathSeparator s ++ [pathSeparator]
      parselexspec = parserLexerSelector tpar
      lexfun = cf2lex $ lexer parselexspec
      parsefun = cf2parse $ parser parselexspec
      parmake = (makeparserdetails (parser parselexspec)) tpar
      lexmake = (makelexerdetails (lexer parselexspec)) tpar
      tpar = ToolParams{
              commentString = "#",
              multilineComment = \x -> "\"\"\"" ++ x ++ "\"\"\"",
              preservePositions = linenumbers,
              packageAbsyn = packBase,
              packageBase = packBase,
              generateAction = BNFC.Backend.Python.AntlrAdapter.generateAntlrAction,
              targetReservedWords = pythonReserved++(targetReservedWords defaultAntlrParameters),
              lexerHeader = "",
              parserHeader= BNFC.Backend.Python.AntlrAdapter.pyAntlrHeader,
              lexerMembers= "",
              parserMembers= BNFC.Backend.Python.AntlrAdapter.pyAntlrMembers
            } 

makefile ::  ToolParameters -> FilePath -> ParserLexerSpecification -> Doc
makefile  tpar dirBase jlexpar = vcat $
    makeVars [  ("JAVAC", "javac"),
                ("JAVAC_FLAGS", "-sourcepath ."),
                ( "JAVA", "java"),
                ( "JAVA_FLAGS", ""),
            -- parser executable
                ( "PARSER", executable parmake),
            -- parser flags
                ( "PARSER_FLAGS", flags parmake dirBase),
             -- lexer executable (and flags?)
                ( "LEXER", executable lexmake),
                ( "LEXER_FLAGS", flags lexmake dirBase)
    ]
    ++
    makeRules [ ("all", [ "test" ], []),
                ( "test", "absyn" : classes, []),
                ( ".PHONY", ["absyn"],     [])
                ]++
    [-- running the lexergen: output of lexer -> input of lexer : calls lexer
    let ff = filename lexmake -- name of input file without extension
        dirBaseff = dirBase ++ ff -- prepend directory
        inp = dirBase ++ inputfile lexmake in
            mkRule (dirBaseff +.+ "py") [ inp ]
            [ "${LEXER} ${LEXER_FLAGS} "++ inp ]

    -- running the parsergen, these there are its outputs
    -- output of parser -> input of parser : calls parser
  , let inp = dirBase ++ inputfile parmake in
        mkRule (unwords (map (dirBase++) (dotPy $ results parmake)))
          [ inp ] $
          ("${PARSER} ${PARSER_FLAGS} " ++ inp) :
          ["mv " ++ unwords (dotPy $ results parmake) +++ dirBase
              | moveresults parmake]
  -- Class of the output of lexer generator wants java of :
  -- output of lexer and parser generator
  , let lexerOutClass = dirBase ++ filename lexmake +.+ "pyc"
        outname x = dirBase ++ x +.+ "py"
        deps = map outname (results lexmake ++ results parmake) in
          mkRule lexerOutClass deps []
    ]++
  reverse [mkRule tar dep [] | 
    (tar,dep) <- partialParserGoals dirBase (results parmake)]
  ++[ mkRule (dirBase ++ prettyPrinterFileName +.+ "pyc")
        [ dirBase ++ prettyPrinterFileName +.+"py" ] []
    -- Removes all the class files created anywhere
    , mkRule "clean" [] [ "rm -f " ++ dirBase ++ "*.pyc" ]
    -- Remains the same
    , mkRule "distclean" [ "vclean" ] []
    -- removes everything
    , mkRule "vclean" []
        [ " rm -f *.g4 "
          , " rm -f " ++ unwords (map (dirBase ++) $
                      [ inputfile lexmake
                      , inputfile parmake
                      ]
                      ++ dotPy (results lexmake)
                      ++ dotPyc (results lexmake)
                      ++ dotPy generated
                      ++ dotPyc generated
                      ++ dotPy (results parmake)
                      ++ dotPyc (results parmake)
                      ++["*.pyc"])
          , " rm -f Makefile"
          , if null dirBase then "" else " rmdir -p " ++ dirBase ]
    ]
    where
      makeVars x = [mkVar n v | (n,v) <- x]
      makeRules x = [mkRule tar dep recipe  | (tar, dep, recipe) <- x]
      parmake           = (makeparserdetails (parser jlexpar)) tpar
      lexmake           = (makelexerdetails (lexer jlexpar)) tpar
      classes = prependPath dirBase lst
      lst = dotPyc (results lexmake) ++ 
           dotPyc (results parmake)

type TestClass = String
    -- ^ class of the lexer
    -> String
    -- ^ class of the parser
    -> String
    -- ^ package where the non-abstract syntax classes are created
    -> CF
    -- ^ the CF bundle
    -> String

data ParserLexerSpecification = ParseLexSpec
    { parser    :: CFToParser
    , lexer     :: CFToLexer
    , testclass :: TestClass
    }

-- | Test class details for ANTLR4
antlrtest :: ToolParameters -> TestClass
antlrtest tpar le pa _ cf = render $ absVcat $ testscript le pa def $ unwords alternatives 
        where showOpts [] = [] 
              showOpts (x:xs) | normCat x /= x = showOpts xs
                              | otherwise      =  (firstLowerCase $ identCat x) : showOpts xs
              alternatives   = map getNames (showOpts eps) 
              eps            = allEntryPoints cf
              def            = getRuleName tpar $ head $ showOpts eps
              getNames       = (","++) . (getRuleName tpar)
              


testscript :: String -> String -> String -> String -> [Entity]
testscript _ _ [] _ = [NothingPython]
testscript le pa (entri:es) alternatives = [
    Import $ Ident "sys"
    , From (Ident le)
    , From (Ident pa)
    , From (Ident prettyPrinterFileName)
    , Class (Ident testErrorId) (YesInherit $ Ident "BaseException")
    , classMethodDefinition Init [emsg, eli, eco] $ assigningConstructorBody [msg, li, co]
    , Class (Ident strBnfcErrorListener) (YesInherit $ Ident "DiagnosticErrorListener")
    , classMethodDefinition 
        reportAmbiguity [r, d, start, stop, e, a, c] [
            Raise $ testerror [
                        Formatting 
                            "Ambiguity at indexes start=%s stop=%s" 
                                    (tupleLiteral [start, stop]) 
                        ]
        ]
    , classMethodDefinition 
        syntaxError [r, o , l, c, m, e] [
            Raise $ testerror [m, l, c]
            ]
    , classMethodDefinition 
        reportAttemptingFullContext [r,d,start,stop, cA, con] [
            Pass
            ]
    , classMethodDefinition 
        reportContextSensitivity [r,d,start,stop, pred, con] [
            Pass
            ]
    , NothingPython
    , NothingPython]++
     ifCascade [(Equals NameField $ pyStringLiteral "__main__"
        , [Try
            , IndentedBlock [
                input =:= (callFilestream [argv1])
                , lexer =:= (callLexerObject [input])
                , lis =:= (callBnfcErrorListener [])
                , callAddErrorListenerOnLexer [lis]
                , stream =:= callCommonTokenStream [lexer]
                    ]
            , Except
            , IndentedBlock [
                pyPrint $  Formatting "Error: File not found: %s" argv1
                , exit1    
                ]
            , parser =:= (callParserObject [stream])
            , callAddErrorListenerOnParser [lis]
            , PyComment $ "Available entry points: "++alternatives
            , tree =:= callEntry []
            , tok =:= nextToken]++
            ifCascade [(NoEquals tokType minusOne, [
                Raise $ testerror [(pyStringLiteral "Stream does not end with EOF"), tokline, tokcolumn ]
                ])]
            ++[
            pp =:= callPrettyPrinter []
            , pyPrintConstant ""
            , pyPrintConstant "Parse Successful!"
            , pyPrintConstant ""
            , pyPrintConstant "[Abstract Syntax]"
            , pyPrintConstant ""
            , pyPrint $ ppShow [getresult]
            , pyPrintConstant "[Linearized Tree]"
            , pyPrintConstant ""
            , pyPrint $ ppPrint [getresult]
            ]
        )]
    
    where
        [input, parser, lexer, lis, stream, tree, 
            tok, type_, tokenSource, _token, getInputStream, line, column, sys,
            argv, one, exit, filestream, lexerObject, parserObject, bnfcErrorListener, 
            commonTokenStream, prettyPrinter, entryMethod, minusOne, result]
                = map mkId ["input", "parser", "lexer", "lis", "stream", "tree", 
                    "tok", "type", "tokenSource", "_token", "getInputStream",
                    "line", "column", "sys", "argv", "1", "exit",
                    "FileStream", le, pa, strBnfcErrorListener, 
                    "CommonTokenStream", "PrettyPrinter", (toLower entri:es), "-1", "result"]
        tokType = toNames [tok, type_]
        getresult = toNames [tree , result]
        [callFilestream, callLexerObject, callParserObject,
            callBnfcErrorListener, callCommonTokenStream, callPrettyPrinter, 
            callAddErrorListenerOnParser, callAddErrorListenerOnLexer, callEntry,
            callGetInputStream, ppShow, ppPrint] 
                = map (\x -> \y -> Function x y) [filestream, lexerObject, parserObject,
                                                 bnfcErrorListener, commonTokenStream, prettyPrinter,
                                                 addErrorListenerTo parser, addErrorListenerTo lexer,
                                                 toNames [parser, entryMethod],
                                                 toNames [parser, getInputStream],
                                                 
                                                 toNames [pp, showId],
                                                 toNames [pp, pprintId]
                                                 ]
        nextToken = toNames [callGetInputStream [], tokenSource ,_token]
        strBnfcErrorListener = "BNFCErrorListener"
        addErrorListenerTo subj = toNames [subj, addErrorListener]
        tokline = toNames [tok, line]
        tokcolumn = toNames [tok, column]
        exit1 = Function (toNames [sys, exit]) [one]
        emsg = mkId msg
        eli = mkId li
        eco = mkId co
        argv1 = toNames [sys, (SquareBracketAccess argv (YesArray one))]
        msg = "msg"
        li = "li"
        co = "co"
        testErrorId = "TestError"
        [r, o, l, m, d 
            , start, stop, e, a, c
            , cA, pred, con] 
                = map mkId ["r", "o", "l", "m", "d"
                        , "start", "stop", "e", "a", "c"
                        , "cA", "pred", "con"] 
        [reportAmbiguity, syntaxError, addErrorListener
            ,reportAttemptingFullContext, reportContextSensitivity] = 
            map mkId ["reportAmbiguity" , "syntaxError", "addErrorListener"
                        ,"reportAttemptingFullContext", "reportContextSensitivity"]
        testerror args = Function (mkId "TestError") args
    

     


parserLexerSelector :: ToolParameters -> ParserLexerSpecification
parserLexerSelector tpar = ParseLexSpec
    { lexer     = BNFC.Backend.Python.cf2AntlrLex 
    , parser    = BNFC.Backend.Python.cf2AntlrParse 
    , testclass = antlrtest tpar
    }





-- | Instances of cf-lexergen bridges

cf2AntlrLex :: CFToLexer
cf2AntlrLex = CF2Lex
               { cf2lex           =
                BNFC.Backend.Common.Antlr4.CFtoAntlr4Lexer.cf2AntlrLex
               , makelexerdetails = antlrmakedetails "Lexer"
               }

cf2AntlrParse :: CFToParser
cf2AntlrParse = CF2Parse
                { cf2parse          =
                    BNFC.Backend.Common.Antlr4.CFtoAntlr4Parser.cf2AntlrParse
                , makeparserdetails = antlrmakedetails "Parser"
                }


-- | shorthand for Makefile command running javac or java
runJava:: String -> String
runJava   = mkRunProgram "JAVA"

-- | function returning a string executing a program contained in a variable j
-- on input s
mkRunProgram :: String -> String -> String
mkRunProgram j s = refVar j +++ refVar (j +-+ "FLAGS") +++ s



antlrmakedetails :: String -> ToolParameters -> MakeFileDetails
antlrmakedetails typ tpar = MakeDetails
    { executable = runJava "org.antlr.v4.Tool"
    , flags               = \_ -> unwords ["-Dlanguage=Python3"]
    , filename            = classname
    , fileextension       = "g4"
    , toolname            = "ANTLRv4"
    , toolversion         = "4.5.1"
    , supportsEntryPoints = True
    , results             = [classname]
    , moveresults         = False
    }
    where
        classname = (packageBase tpar)++typ

prependPath , appendExtension :: String -> [String] -> [String]
prependPath s fi     = [s ++ x | x<- fi]
appendExtension s fi = [x+.+s | x<- fi]

dotPy :: [String] -> [String]
dotPy = appendExtension "py"
dotPyc = appendExtension "pyc"

type CFtoPython = String ->  CF -> String
-- | Contains the pairs filename/content for all the files
-- generated by BNFC
data BNFCGeneratedEntities = BNFCGenerated
    { bprettyprinter :: (String, String)
    , btest          :: (String, String)
    , babsyn      :: (String, String)
    , bskel          :: (String,  String)
    }

generated = [prettyPrinterFileName, visitorFileName, absynFileName, testFileName]  
[prettyPrinterFileName, visitorFileName, absynFileName, testFileName] =
    ["prettyprinter", "visitor","absyn", "test"]

bnfcVisitorsAndTests :: String   -> CF      ->
                        CFtoPython -> CFtoPython -> CFtoPython ->
                        CFtoPython -> BNFCGeneratedEntities
bnfcVisitorsAndTests pbase cf cf0 cf1 cf2 cf3 =
    BNFCGenerated{ 
        bprettyprinter = ( prettyPrinterFileName , app cf0)
        , bskel          = ( visitorFileName, app cf1)
        , babsyn         = ( absynFileName , app cf2)
        , btest          = ( testFileName , app cf3)
    }
      where app x = x pbase cf

inputfile x = filename x ++ case fileextension x of
                                "" -> ""
                                a -> '.':a

-- |  constructs the rules regarding the parser in the makefile
partialParserGoals :: String -> [String] -> [(String, [String])]
partialParserGoals _ []          = []
partialParserGoals dbas (x:rest) =
    (dbas++x+.+"pyc",map (\y ->dbas++y+.+"py")(x:rest))
        :partialParserGoals dbas rest

