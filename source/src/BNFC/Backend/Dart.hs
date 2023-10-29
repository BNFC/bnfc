{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module BNFC.Backend.Dart ( makeDart ) where

import Prelude hiding ((<>))

import System.FilePath ((</>), (<.>), pathSeparator, isPathSeparator)
import System.Cmd (system)
import Data.Foldable (toList)
import Data.List ( intersperse )

import BNFC.Utils
import BNFC.CF
import BNFC.Options as Options
import BNFC.Backend.Base
import BNFC.Backend.Java.Utils
import BNFC.Backend.Java.CFtoCup15 ( cf2Cup )
import BNFC.Backend.Java.CFtoJLex15
import BNFC.Backend.Java.CFtoAntlr4Lexer
import BNFC.Backend.Java.CFtoAntlr4Parser
import BNFC.Backend.Dart.CFtoDartAbs ( cf2DartAbs )
import BNFC.Backend.Java.CFtoJavaPrinter15
import BNFC.Backend.Java.CFtoVisitSkel15
import BNFC.Backend.Java.CFtoComposVisitor
import BNFC.Backend.Java.CFtoAbstractVisitor
import BNFC.Backend.Java.CFtoFoldVisitor
import BNFC.Backend.Java.CFtoAllVisitor
import BNFC.Backend.Common.NamedVariables (SymEnv, firstLowerCase)
import qualified BNFC.Backend.Common.Makefile as Makefile
import BNFC.PrettyPrint


makeDart :: SharedOptions -> CF -> MkFiles ()
makeDart opt = makeDart' pkg opt{ lang = lang' }
  where
    pkg   = mkName javaReserved SnakeCase $ lang opt
    lang' = capitalize $ mkName javaReserved CamelCase $ lang opt

makeDart' :: String -> SharedOptions -> CF -> MkFiles ()
makeDart' pkg options@Options{..} cf = do
     -- Create the package directories if necessary.
    let packageBase  = maybe id (+.+) inPackage pkg
        packageAbsyn = packageBase +.+ "Absyn"
        dirBase      = pkgToDir packageBase
        dirAbsyn     = pkgToDir packageAbsyn
        javaex str   = dirBase </> str <.> "dart"
        bnfcfiles    =
          bnfcVisitorsAndTests
            packageBase
            packageAbsyn
            cf
            cf2JavaPrinter
            cf2VisitSkel
            cf2ComposVisitor
            cf2AbstractVisitor
            cf2FoldVisitor
            cf2AllVisitor
            (testclass parselexspec
                (head $ results lexmake) -- lexer class
                (head $ results parmake) -- parser class
            )
        makebnfcfile x = mkfile (javaex (fst $ x bnfcfiles)) comment
                                        (snd $ x bnfcfiles)
    let absynContent = cf2DartAbs cf rp
        absynDir = dirAbsyn ++ ".dart"
        absynFileNames = [ absynDir ]
    mkfile absynDir comment absynContent
    -- makebnfcfile bprettyprinter
    -- makebnfcfile bskel
    -- makebnfcfile bcompos
    -- makebnfcfile babstract
    -- makebnfcfile bfold
    -- makebnfcfile ball
    -- makebnfcfile btest
    -- let (lex, env) = lexfun packageBase cf
    -- -- Where the lexer file is created. lex is the content!
    -- mkfile (dirBase </> inputfile lexmake ) commentWithEmacsModeHint lex
    -- liftIO $ putStrLn $ "   (Tested with" +++ toolname lexmake
    --                                       +++ toolversion lexmake  ++ ")"
    -- -- where the parser file is created.
    -- mkfile (dirBase </> inputfile parmake) commentWithEmacsModeHint
    --       $ parsefun packageBase packageAbsyn cf rp env
    -- liftIO $ putStrLn $
    --   if supportsEntryPoints parmake
    --    then "(Parser created for all categories)"
    --    else "   (Parser created only for category " ++ prettyShow (firstEntry cf) ++ ")"
    -- liftIO $ putStrLn $ "   (Tested with"  +++ toolname parmake
    --                                        +++ toolversion parmake ++ ")"
    -- Makefile.mkMakefile optMake $
    --     makefile dirBase dirAbsyn absynFileNames parselexspec
  where
    remDups [] = []
    remDups ((a,b):as) = case lookup a as of
                           Just {} -> remDups as
                           Nothing -> (a, b) : remDups as
    pkgToDir :: String -> FilePath
    pkgToDir = replace '.' pathSeparator

    parselexspec = parserLexerSelector lang javaLexerParser rp
    lexfun       = cf2lex $ lexer parselexspec
    parsefun     = cf2parse $ parser parselexspec
    parmake      = makeparserdetails (parser parselexspec)
    lexmake      = makelexerdetails  (lexer parselexspec)
    rp           = (Options.linenumbers options)
    commentWithEmacsModeHint = comment . ("-*- Java -*- " ++)

makefile ::  FilePath -> FilePath -> [String] -> ParserLexerSpecification -> String -> Doc
makefile  dirBase dirAbsyn absynFileNames jlexpar basename = vcat $
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
        dirBaseff = dirBase </> ff -- prepend directory
        inp = dirBase </> inputfile lexmake in
        Makefile.mkRule (dirBaseff <.> "java") [ inp ]
        [ "${LEXER} ${LEXER_FLAGS} "++ inp ]

    -- running the parsergen, these there are its outputs
    -- output of parser -> input of parser : calls parser
  , let inp = dirBase </> inputfile parmake in
        Makefile.mkRule (unwords (map (dirBase </>) (dotJava $ results parmake)))
          [ inp ] $
          ("${PARSER} ${PARSER_FLAGS} " ++ inp) :
          ["mv " ++ unwords (dotJava $ results parmake) +++ dirBase ++ [pathSeparator]
              | moveresults parmake]
  -- Class of the output of lexer generator wants java of :
  -- output of lexer and parser generator
  , let lexerOutClass = dirBase </> filename lexmake <.> "class"
        outname x = dirBase </> x <.> "java"
        deps = map outname (results lexmake ++ results parmake) in
        Makefile.mkRule lexerOutClass deps []
    ]++
  reverse [Makefile.mkRule tar dep [] |
    (tar,dep) <- partialParserGoals dirBase (results parmake)]
  ++[ Makefile.mkRule (dirBase </> "PrettyPrinter.class")
        [ dirBase </> "PrettyPrinter.java" ] []
    -- Removes all the class files created anywhere
    , Makefile.mkRule "clean" [] [ "rm -f " ++ dirAbsyn </> "*.class" ++ " "
                                            ++ dirBase </> "*.class" ]
    -- Remains the same
    , Makefile.mkRule "distclean" [ "vclean" ] []
    -- removes everything
    , Makefile.mkRule "vclean" []
        [ " rm -f " ++ absynJavaSrc ++ " " ++ absynJavaClass
        , " rm -f " ++ dirAbsyn </> "*.class"
        , " rmdir " ++ dirAbsyn
        , " rm -f " ++ unwords (map (dirBase </>) $
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
                    ++ ["*.class"]
                    ++ other_results lexmake
                    ++ other_results parmake)
        , " rm -f " ++ basename
        , " rmdir -p " ++ dirBase
        ]
    ]
    where
      makeVars x = [Makefile.mkVar n v | (n,v) <- x]
      makeRules x = [Makefile.mkRule tar dep recipe  | (tar, dep, recipe) <- x]
      parmake           = makeparserdetails (parser jlexpar)
      lexmake           = makelexerdetails (lexer jlexpar)
      absynJavaSrc      = unwords (dotJava absynFileNames)
      absynJavaClass    = unwords (dotClass absynFileNames)
      classes = map (dirBase </>) lst
      lst = dotClass (results lexmake) ++ [ "PrettyPrinter.class", "Test.class"
          , "VisitSkel.class"
          , "ComposVisitor.class", "AbstractVisitor.class"
          , "FoldVisitor.class", "AllVisitor.class"]++
           dotClass (results parmake) ++ ["Test.class"]

type TestClass = String
    -- ^ class of the lexer
    -> String
    -- ^ class of the parser
    -> String
    -- ^ package where the non-abstract syntax classes are created
    -> String
    -- ^ package where the abstract syntax classes are created
    -> CF
    -- ^ the CF bundle
    -> String

-- | Record to name arguments of 'javaTest'.
data JavaTestParams = JavaTestParams
  { jtpImports            :: [Doc]
      -- ^ List of imported packages.
  , jtpErr                :: String
      -- ^ Name of the exception thrown in case of parsing failure.
  , jtpErrHand            :: (String -> [Doc])
      -- ^ Handler for the exception thrown.
  , jtpLexerConstruction  :: (Doc -> Doc -> Doc)
      -- ^ Function formulating the construction of the lexer object.
  , jtpParserConstruction :: (Doc -> Doc -> Doc)
      -- ^ As above, for parser object.
  , jtpShowAlternatives   :: ([Cat] -> [Doc])
      -- ^ Pretty-print the names of the methods corresponding to entry points to the user.
  , jtpInvocation         :: (Doc -> Doc -> Doc -> Doc -> Doc)
      -- ^ Function formulating the invocation of the parser tool within Java.
  , jtpErrMsg             :: String
      -- ^ Error string output in consequence of a parsing failure.
  }

-- | Test class details for J(F)Lex + CUP
cuptest :: TestClass
cuptest = javaTest $ JavaTestParams
  { jtpImports            = ["java_cup.runtime"]
  , jtpErr                = "Throwable"
  , jtpErrHand            = const []
  , jtpLexerConstruction  = \ x i -> x <> i <> ";"
  , jtpParserConstruction = \ x i -> x <> "(" <> i <> ", " <> i <> ".getSymbolFactory());"
  , jtpShowAlternatives   = const $ ["not available."]
  , jtpInvocation         = \ _ pabs dat enti -> hcat [ pabs, ".", dat, " ast = p.p", enti, "();" ]
  , jtpErrMsg             = unwords $
      [ "At line \" + String.valueOf(t.l.line_num()) + \","
      , "near \\\"\" + t.l.buff() + \"\\\" :"
      ]
  }

-- | Test class details for ANTLR4
antlrtest :: TestClass
antlrtest = javaTest $ JavaTestParams
  { jtpImports =
      [ "org.antlr.v4.runtime"
      , "org.antlr.v4.runtime.atn"
      , "org.antlr.v4.runtime.dfa"
      , "java.util"
      ]
  , jtpErr =
      "TestError"
  , jtpErrHand =
      antlrErrorHandling
  , jtpLexerConstruction  =
      \ x i -> vcat
        [ x <> "(new ANTLRInputStream" <> i <>");"
        , "l.addErrorListener(new BNFCErrorListener());"
        ]
  , jtpParserConstruction =
      \ x i -> vcat
        [ x <> "(new CommonTokenStream(" <> i <>"));"
        , "p.addErrorListener(new BNFCErrorListener());"
        ]
  , jtpShowAlternatives   =
      showOpts
  , jtpInvocation         =
      \ pbase pabs dat enti -> vcat
         [
           let rulename = getRuleName $ startSymbol $ render enti
               typename = text rulename
               methodname = text $ firstLowerCase rulename
           in
               pbase <> "." <> typename <> "Context pc = p." <> methodname <> "();"
         , pabs <> "." <> dat <+> "ast = pc.result;"
         ]
  , jtpErrMsg             =
      "At line \" + e.line + \", column \" + e.column + \" :"
  }
  where
    showOpts [] = []
    showOpts (x:xs)
      | normCat x /= x = showOpts xs
      | otherwise      = text (firstLowerCase $ identCat x) : showOpts xs

parserLexerSelector ::
       String
    -> JavaLexerParser
    -> RecordPositions -- ^Pass line numbers to the symbols
    -> ParserLexerSpecification
parserLexerSelector _ JLexCup rp = ParseLexSpec
    { lexer     = cf2JLex rp
    , parser    = cf2cup rp
    , testclass = cuptest
    }
parserLexerSelector _ JFlexCup rp =
    (parserLexerSelector "" JLexCup rp){lexer = cf2JFlex rp}
parserLexerSelector l Antlr4 _ = ParseLexSpec
    { lexer     = cf2AntlrLex' l
    , parser    = cf2AntlrParse' l
    , testclass = antlrtest
    }

data ParserLexerSpecification = ParseLexSpec
    { parser    :: CFToParser
    , lexer     :: CFToLexer
    , testclass :: TestClass
    }

-- |CF -> LEXER GENERATION TOOL BRIDGE
-- | function translating the CF to an appropriate lexer generation tool.
type CF2LexerFunction = String -> CF -> (Doc, SymEnv)

-- Chooses the translation from CF to the lexer
data CFToLexer = CF2Lex
    { cf2lex           :: CF2LexerFunction
    , makelexerdetails :: MakeFileDetails
    }

-- | Instances of cf-lexergen bridges

cf2JLex :: RecordPositions -> CFToLexer
cf2JLex rp = CF2Lex
  { cf2lex           = cf2jlex JLexCup rp
  , makelexerdetails = jlexmakedetails
  }

cf2JFlex :: RecordPositions -> CFToLexer
cf2JFlex rp = CF2Lex
  { cf2lex           = cf2jlex JFlexCup rp
  , makelexerdetails = jflexmakedetails
  }

cf2AntlrLex' :: String -> CFToLexer
cf2AntlrLex' l = CF2Lex
  { cf2lex           = const $ cf2AntlrLex l
  , makelexerdetails = antlrmakedetails $ l ++ "Lexer"
  }

-- | CF -> PARSER GENERATION TOOL BRIDGE
-- | function translating the CF to an appropriate parser generation tool.
type CF2ParserFunction = String -> String -> CF -> RecordPositions -> SymEnv -> String

-- | Chooses the translation from CF to the parser
data CFToParser = CF2Parse
    { cf2parse          :: CF2ParserFunction
    , makeparserdetails :: MakeFileDetails
    }

-- | Instances of cf-parsergen bridges
cf2cup :: RecordPositions -> CFToParser
cf2cup rp = CF2Parse
    { cf2parse          = cf2Cup
    , makeparserdetails = cupmakedetails rp
    }

cf2AntlrParse' :: String -> CFToParser
cf2AntlrParse' l = CF2Parse
  { cf2parse          = const $ cf2AntlrParse l
  , makeparserdetails = antlrmakedetails $ l ++ "Parser"
  }


-- | shorthand for Makefile command running javac or java
runJavac , runJava:: String -> String
runJava   = mkRunProgram "JAVA"
runJavac  = mkRunProgram "JAVAC"

-- | function returning a string executing a program contained in a variable j
-- on input s
mkRunProgram :: String -> String -> String
mkRunProgram j s = Makefile.refVar j +++ Makefile.refVar (j +-+ "FLAGS") +++ s

type OutputDirectory = String

-- | Makefile details from running the parser-lexer generation tools.
data MakeFileDetails = MakeDetails
    { -- | The string that executes the generation tool.
      executable          :: String
    , -- | Flags to pass to the tool.
      flags               :: OutputDirectory -> String
    , -- | Input file to the tool.
      filename            :: String
    , -- | Extension of input file to the tool.
      fileextension       :: String
    , -- | Name of the tool.
      toolname            :: String
    , -- | Tool version.
      toolversion         :: String
    , -- | True if the tool is a parser and supports entry points,
      --   False otherwise.
      supportsEntryPoints :: Bool
    , -- | List of names (without extension!) of files resulting from the
      --   application of the tool which are relevant to a make rule.
      results             :: [String]
    , -- | List of names of files resulting from the application of
      --   the tool which are irrelevant to the make rules but need to be cleaned.
      other_results       :: [String]
    , -- | If True, the files are moved to the base directory, otherwise
      -- they are left where they are.
      moveresults         :: Bool
    }


-- Instances of makefile details.

jlexmakedetails :: MakeFileDetails
jlexmakedetails = MakeDetails
    { executable          = runJava "JLex.Main"
    , flags               = const ""
    , filename            = "Yylex"
    , fileextension       = ""
    , toolname            = "JLex"
    , toolversion         = "1.2.6"
    , supportsEntryPoints = False
    , results             = ["Yylex"]
    , other_results       = []
    , moveresults         = False
    }

jflexmakedetails :: MakeFileDetails
jflexmakedetails = jlexmakedetails
    { executable  = "jflex"
    , toolname    = "JFlex"
    , toolversion = "1.4.3 - 1.9.1"
    }

cupmakedetails :: RecordPositions -> MakeFileDetails
cupmakedetails rp = MakeDetails
    { executable          = runJava "java_cup.Main"
    , flags               = const (lnFlags ++ " -expect 100")
    , filename            = "_cup"
    , fileextension       = "cup"
    , toolname            = "CUP"
    , toolversion         = "0.11b"
    , supportsEntryPoints = False
    , results             = ["parser", "sym"]
    , other_results       = []
    , moveresults         = True
    }
  where
    lnFlags = if rp == RecordPositions then "-locations" else "-nopositions"


antlrmakedetails :: String -> MakeFileDetails
antlrmakedetails l = MakeDetails
    { executable = runJava "org.antlr.v4.Tool"
    , flags               = \ path -> unwords $
                                    let pointed = map cnv path
                                        cnv y   = if isPathSeparator y
                                                        then '.'
                                                        else y
                                        in [ "-lib", path
                                           , "-package", pointed]
    , filename            = l
    , fileextension       = "g4"
    , toolname            = "ANTLRv4"
    , toolversion         = "4.9"
    , supportsEntryPoints = True
    , results             = [l]
    , other_results       = map (l ++)
        [ ".interp"              -- added after ANTLR 4.5
        , ".tokens"
        , "BaseListener.java"
        ,"Listener.java"
        ]
    , moveresults         = False
    }

dotJava :: [String] -> [String]
dotJava  = map (<.> "java")

dotClass :: [String] -> [String]
dotClass = map (<.> "class")

type CFToJava = String -> String -> CF -> String

-- | Contains the pairs filename/content for all the non-abstract syntax files
-- generated by BNFC.
data BNFCGeneratedEntities = BNFCGenerated
    { bprettyprinter :: (String, String)
    , btest          :: (String, String)
    , bcompos        :: (String, String)
    , babstract      :: (String, String)
    , bfold          :: (String, String)
    , ball           :: (String, String)
    , bskel          :: (String, String)
    }

bnfcVisitorsAndTests :: String   -> String    -> CF      ->
                        CFToJava -> CFToJava -> CFToJava ->
                        CFToJava -> CFToJava -> CFToJava ->
                        CFToJava -> BNFCGeneratedEntities
bnfcVisitorsAndTests pbase pabsyn cf cf0 cf1 cf2 cf3 cf4 cf5 cf6 =
    BNFCGenerated
    { bprettyprinter = ( "PrettyPrinter" , app cf0)
    , bskel          = ( "VisitSkel", app cf1)
    , bcompos        = ( "ComposVisitor" , app cf2)
    , babstract      = ( "AbstractVisitor" , app cf3)
    , bfold          = ( "FoldVisitor", app cf4)
    , ball           = ( "AllVisitor", app cf5)
    , btest          = ( "Test" , app cf6)
    }
  where app x = x pbase pabsyn cf

inputfile :: MakeFileDetails -> String
inputfile x
  | null (fileextension x) = filename x
  | otherwise              = filename x <.> fileextension x

-- |  constructs the rules regarding the parser in the makefile
partialParserGoals :: String -> [String] -> [(String, [String])]
partialParserGoals _ []          = []
partialParserGoals dirBase (x:rest) =
    (dirBase </> x <.> "class", map (\ y -> dirBase </> y <.> "java") (x:rest))
        : partialParserGoals dirBase rest

-- | Creates the Test.java class.
javaTest :: JavaTestParams -> TestClass
javaTest (JavaTestParams
    imports
    err
    errhand
    lexerconstruction
    parserconstruction
    showOpts
    invocation
    errmsg)
    lexer
    parser
    packageBase
    packageAbsyn
    cf =
    render $ vcat $ concat $
      [ [ "package" <+> text packageBase <> ";"
        , ""
        , "import" <+> text packageBase <> ".*;"
        , "import java.io.*;"
        ]
      , map importfun imports
      , [ "" ]
      , errhand err
      , [ ""
        , "public class Test"
        , codeblock 2
            [ lx <+> "l;"
            , px <+> "p;"
            , ""
            , "public Test(String[] args)"
            , codeblock 2
                [ "try"
                , codeblock 2
                    [ "Reader input;"
                    , "if (args.length == 0) input = new InputStreamReader(System.in);"
                    , "else input = new FileReader(args[0]);"
                    , "l = new " <> lexerconstruction lx "(input)"
                    ]
                , "catch(IOException e)"
                , codeblock 2
                    [ "System.err.println(\"Error: File not found: \" + args[0]);"
                    , "System.exit(1);"
                    ]
                , "p = new "<> parserconstruction px "l"
                ]
            , ""
            , "public" <+> text packageAbsyn <> "." <> dat
                <+> "parse() throws Exception"
            , codeblock 2 $ concat
                [ [ "/* The default parser is the first-defined entry point. */" ]
                , unlessNull (drop 1 eps) $ \ eps' ->
                  [ "/* Other options are: */"
                  , "/* " <> fsep (punctuate "," (showOpts eps')) <> " */"
                  ]
                , [ invocation px (text packageAbsyn) dat absentity
                  , printOuts
                     [ "\"Parse Successful!\""
                     , "\"[Abstract Syntax]\""
                     , "PrettyPrinter.show(ast)"
                     , "\"[Linearized Tree]\""
                     , "PrettyPrinter.print(ast)"
                     ]
                  , "return ast;"
                  ]
                ]
            , ""
            , "public static void main(String args[]) throws Exception"
            , codeblock 2
                [ "Test t = new Test(args);"
                , "try"
                , codeblock 2 [ "t.parse();" ]
                ,"catch(" <> text err <+> "e)"
                , codeblock 2
                    [ "System.err.println(\"" <> text errmsg <> "\");"
                    , "System.err.println(\"     \" + e.getMessage());"
                    , "System.exit(1);"
                    ]
                ]
            ]
        ]
      ]
    where
      printOuts x    = vcat $ map javaPrintOut (messages x)
      messages x     = "" : intersperse "" x
      javaPrintOut x = text $ "System.out.println(" ++ x ++ ");"
      importfun x    = "import" <+> x <> ".*;"
      lx             = text lexer
      px             = text parser
      dat            = text $ identCat $ normCat def  -- Use for AST types.
      absentity      = text $ identCat def            -- Use for parser/printer name.
      eps            = toList $ allEntryPoints cf
      def            = head eps

-- | Error handling in ANTLR.
-- By default, ANTLR does not stop after any parsing error and attempts to go
-- on, delivering what it has been able to parse.
-- It does not throw any exception, unlike J(F)lex+CUP.
-- The below code makes the test class behave as with J(F)lex+CUP.
antlrErrorHandling :: String -> [Doc]
antlrErrorHandling te =
    [ "class"<+>tedoc<+>"extends RuntimeException"
    , codeblock 2 [ "int line;"
        , "int column;"
        , "public"<+>tedoc<>"(String msg, int l, int c)"
        , codeblock 2 [ "super(msg);"
            , "line = l;"
            , "column = c;"
            ]
        ]
    , "class BNFCErrorListener implements ANTLRErrorListener"
    , codeblock 2 [ "@Override"
        , "public void syntaxError(Recognizer<?, ?> recognizer, Object o, int i"
            <> ", int i1, String s, RecognitionException e)"
        , codeblock 2 [ "throw new"<+>tedoc<>"(s,i,i1);"]
        , "@Override"
        , "public void reportAmbiguity(Parser parser, DFA dfa, int i, int i1, "
            <>"boolean b, BitSet bitSet, ATNConfigSet atnConfigSet)"
        , codeblock 2[ "throw new"<+>tedoc<>"(\"Ambiguity at\",i,i1);" ]
        , "@Override"
        , "public void reportAttemptingFullContext(Parser parser, DFA dfa, "
            <>"int i, int i1, BitSet bitSet, ATNConfigSet atnConfigSet)"
        , codeblock 2 []
        , "@Override"
        ,"public void reportContextSensitivity(Parser parser, DFA dfa, int i, "
            <>"int i1, int i2, ATNConfigSet atnConfigSet)"
        ,codeblock 2 []
        ]
    ]
    where tedoc = text te
