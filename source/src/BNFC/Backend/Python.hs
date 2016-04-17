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
import System.FilePath (pathSeparator, isPathSeparator)
import Data.List ( intersperse )
import BNFC.Utils
import BNFC.CF
import BNFC.Options as Options
import BNFC.Backend.Base
import BNFC.Backend.Java.Utils
import BNFC.Backend.Python.AbsPython
import BNFC.Backend.Python.Utils
import BNFC.Backend.Common.Antlr4.CFtoAntlr4Lexer
import BNFC.Backend.Common.Antlr4.CFtoAntlr4Parser
import BNFC.Backend.Python.CFtoPyAbsyn
import BNFC.Backend.Python.CFtoPyPrinter
import BNFC.Backend.Python.CFtoPyVisitSkel
import BNFC.Backend.Common.NamedVariables (SymEnv, firstLowerCase)
import BNFC.Backend.Common.MultipleParserGenerationTools
import BNFC.Backend.Python.AntlrAdapter
import BNFC.Backend.Common.Makefile
import BNFC.PrettyPrint
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
      --todo: visitor prettyprinter class file
      makebnfcfile bprettyprinter      
      --todo: visitor skeleton class file
      makebnfcfile bskel
      -- todo: test file generation
      makebnfcfile btest
      -- todo: lexer and parser input file generation
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
                                             
-- todo : Create makefile
    where
      packBase  = case inPackage of
                             Nothing -> lang
                             Just p -> p ++ "." ++ lang
      dirBase      = pkgToDir packBase
      remDups [] = []
      remDups ((a,b):as) = case lookup a as of
                             Just {} -> remDups as
                             Nothing -> (a, b) : remDups as
      pkgToDir :: String -> FilePath
      pkgToDir s = replace '.' pathSeparator s ++ [pathSeparator]
      parselexspec = parserLexerSelector 
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
              lexerHeader = "",
              parserHeader= BNFC.Backend.Python.AntlrAdapter.pyAntlrHeader,
              lexerMembers= "",
              parserMembers= BNFC.Backend.Python.AntlrAdapter.pyAntlrMembers
            } 

makefile ::  ToolParameters -> FilePath -> FilePath -> [String] -> ParserLexerSpecification -> Doc
makefile  tpar dirBase dirAbsyn absynFileNames jlexpar = vcat $
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
                ( ".PHONY", ["absyn"],     []),
                ("%.class", [ "%.java" ],  [ runJavac "$^" ]),
                ("absyn",   [absynJavaSrc],[ runJavac "$^" ])
                ]++
    [-- running the lexergen: output of lexer -> input of lexer : calls lexer
    let ff = filename lexmake -- name of input file without extension
        dirBaseff = dirBase ++ ff -- prepend directory
        inp = dirBase ++ inputfile lexmake in
            mkRule (dirBaseff +.+ "java") [ inp ]
            [ "${LEXER} ${LEXER_FLAGS} "++ inp ]

    -- running the parsergen, these there are its outputs
    -- output of parser -> input of parser : calls parser
  , let inp = dirBase ++ inputfile parmake in
        mkRule (unwords (map (dirBase++) (dotJava $ results parmake)))
          [ inp ] $
          ("${PARSER} ${PARSER_FLAGS} " ++ inp) :
          ["mv " ++ unwords (dotJava $ results parmake) +++ dirBase
              | moveresults parmake]
  -- Class of the output of lexer generator wants java of :
  -- output of lexer and parser generator
  , let lexerOutClass = dirBase ++ filename lexmake +.+ "class"
        outname x = dirBase ++ x +.+ "java"
        deps = map outname (results lexmake ++ results parmake) in
          mkRule lexerOutClass deps []
    ]++
  reverse [mkRule tar dep [] | 
    (tar,dep) <- partialParserGoals dirBase (results parmake)]
  ++[ mkRule (dirBase ++ "PrettyPrinter.class")
        [ dirBase ++ "PrettyPrinter.java" ] []
    -- Removes all the class files created anywhere
    , mkRule "clean" [] [ "rm -f " ++ dirAbsyn ++ "*.class" ++ " "
                                            ++ dirBase ++ "*.class" ]
    -- Remains the same
    , mkRule "distclean" [ "vclean" ] []
    -- removes everything
    , mkRule "vclean" []
        [ " rm -f " ++ absynJavaSrc ++ " " ++ absynJavaClass
          , " rm -f " ++ dirAbsyn ++ "*.class"
          , " rmdir " ++ dirAbsyn
          , " rm -f " ++ unwords (map (dirBase ++) $
                      [ inputfile lexmake
                      , inputfile parmake
                      ]
                      ++ dotJava (results lexmake)
                      ++ [ "VisitSkel.java"
                        , "ComposVisitor.java"
                        , "AbstractVisitor.java"
                        , "FoldVisitor.java"
                        , "AllVisitor.java"
                        , "PrettyPrinter.java"
                        , "Skeleton.java"
                        , "Test.java"
                        ]
                      ++ dotJava (results parmake)
                      ++["*.class"])
          , " rm -f Makefile"
          , " rmdir -p " ++ dirBase ]
    ]
    where
      makeVars x = [mkVar n v | (n,v) <- x]
      makeRules x = [mkRule tar dep recipe  | (tar, dep, recipe) <- x]
      parmake           = (makeparserdetails (parser jlexpar)) tpar
      lexmake           = (makelexerdetails (lexer jlexpar)) tpar
      absynJavaSrc      = unwords (dotJava absynFileNames)
      absynJavaClass    = unwords (dotClass absynFileNames)
      classes = prependPath dirBase lst
      lst = dotClass (results lexmake) ++ [ "PrettyPrinter.class", "Test.class"
          , "ComposVisitor.class", "AbstractVisitor.class"
          , "FoldVisitor.class", "AllVisitor.class"]++
           dotClass (results parmake) ++ ["Test.class"]

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
antlrtest :: TestClass
antlrtest le pa pack cf = "# this is a test" 
        where showOpts [] = [] 
              showOpts (x:xs) | normCat x /= x = showOpts xs
                              | otherwise      = text (firstLowerCase $ identCat x) : showOpts xs
              eps            = allEntryPoints cf
              absentity      = text $ show def
              def            = head eps
              


testscript :: String -> String -> String -> [Entity]
testscript le pa entry = [
    Import $ Ident "sys"
    , From (Ident le)
    , From (Ident pa)
    , From (Ident "PrettyPrinter")
    , Class (Ident testErrorId) (YesInherit $ Ident "BaseException")
    , classMethodDefinition Init [emsg, eli, eco] $ assigningConstructorBody [msg, li, co]
    , Class (Ident strBnfcErrorListener) (YesInherit $ Ident "DiagnosticErrorListener")
    , classMethodDefinition reportAmbiguity [r, d, start, stop, e, a, c] [
        Raise $ testerror [Formatting "Ambiguity at indexes start=%s stop=%s" (tupleLiteral [start, stop]) ]
        ]
    , classMethodDefinition syntaxError [r, o , l, c, m, e] [
        Raise $ testerror [m, l, c]
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
            , callAddErrorListenerOnLexer [lis]
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
        (=:=) :: Entity -> Entity -> Entity
        (=:=) x y = Assignment [x] [y]
        [input, parser, lexer, lis, stream, tree, 
            tok, type_, tokenSource, _token, getInputStream, line, column, sys,
            argv, one, exit, filestream, lexerObject, parserObject, bnfcErrorListener, 
            commonTokenStream, prettyPrinter, entryMethod, minusOne, result]
                = map mkId ["input", "parser", "lexer", "lis", "stream", "tree", 
                    "tok", "type", "tokenSource", "_token", "getInputStream",
                    "line", "column", "sys", "argv", "1", "exit",
                    "FileStream", le, pa, strBnfcErrorListener, 
                    "CommonTokenStream", "PrettyPrinter", entry, "-1", "result"]
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
        [r, o, l, m, d, start, stop, e, a, c] = map mkId ["r", "o", "l", "m", "d", "start", "stop", "e", "a", "c"] 
        [reportAmbiguity, syntaxError, addErrorListener ] = map mkId ["reportAmbiguity" , "syntaxError", "addErrorListener"]
        testerror args = Function (mkId "TestError") args
    

     


parserLexerSelector :: ParserLexerSpecification
parserLexerSelector = ParseLexSpec
    { lexer     = BNFC.Backend.Python.cf2AntlrLex 
    , parser    = BNFC.Backend.Python.cf2AntlrParse 
    , testclass = antlrtest
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
runJavac , runJava:: String -> String
runJava   = mkRunProgram "JAVA"
runJavac  = mkRunProgram "JAVAC"

-- | function returning a string executing a program contained in a variable j
-- on input s
mkRunProgram :: String -> String -> String
mkRunProgram j s = refVar j +++ refVar (j +-+ "FLAGS") +++ s




mapEmpty :: a->String
mapEmpty _ = ""

antlrmakedetails :: String -> ToolParameters -> MakeFileDetails
antlrmakedetails typ tpar = MakeDetails
    { executable = runJava "org.antlr.v4.Tool"
    , flags               = \x -> unwords $
                                    let path    = take (length x - 1) x
                                        pointed = map cnv path
                                        cnv y   = if isPathSeparator y
                                                        then '.'
                                                        else y
                                        in [ "-lib", path
                                           , "-package", pointed]
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

dotJava,dotClass :: [String] -> [String]
dotJava  = appendExtension "java"
dotClass = appendExtension "class"

type CFtoPython = String ->  CF -> String
-- | Contains the pairs filename/content for all the files
-- generated by BNFC
data BNFCGeneratedEntities = BNFCGenerated
    { bprettyprinter :: (String, String)
    , btest          :: (String, String)
    , babsyn      :: (String, String)
    , bskel          :: (String,  String)
    }

bnfcVisitorsAndTests :: String   -> CF      ->
                        CFtoPython -> CFtoPython -> CFtoPython ->
                        CFtoPython -> BNFCGeneratedEntities
bnfcVisitorsAndTests pbase cf cf0 cf1 cf2 cf3 =
    BNFCGenerated{ 
        bprettyprinter = ( "PrettyPrinter" , app cf0)
        , bskel          = ( "Visitor", app cf1)
        , babsyn         = ( "Absyn" , app cf2)
        , btest          = ( "Test" , app cf3)
    }
      where app x = x pbase cf

inputfile x = filename x ++ case fileextension x of
                                "" -> ""
                                a -> '.':a

-- |  constructs the rules regarding the parser in the makefile
partialParserGoals :: String -> [String] -> [(String, [String])]
partialParserGoals _ []          = []
partialParserGoals dbas (x:rest) =
    (dbas++x+.+"class",map (\y ->dbas++y+.+"java")(x:rest))
        :partialParserGoals dbas rest

