{-
    BNF Converter: Haskell main file
    Copyright (C) 2004  Author:  Markus Forberg, Peter Gammie, Aarne Ranta

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
    Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1335, USA
-}

module BNFC.Backend.Haskell (makeHaskell, AlexVersion(..), makefile, testfile) where

import qualified Control.Monad as Ctrl
import Data.Functor    ((<$>))
import System.FilePath ((<.>), (</>), pathSeparator)
import Text.Printf     (printf)
import Text.PrettyPrint

import BNFC.Backend.Agda
import BNFC.Backend.Base
import BNFC.Backend.Haskell.CFtoHappy
import BNFC.Backend.Haskell.CFtoAlex
import BNFC.Backend.Haskell.CFtoAlex2
import BNFC.Backend.Haskell.CFtoAlex3
import BNFC.Backend.Haskell.CFtoAbstract
import BNFC.Backend.Haskell.CFtoTemplate
import BNFC.Backend.Haskell.CFtoPrinter
import BNFC.Backend.Haskell.CFtoLayout
import BNFC.Backend.Haskell.HsOpts
import BNFC.Backend.Haskell.MkErrM
import BNFC.Backend.Haskell.MkSharedString
import BNFC.Backend.Haskell.ToCNF as ToCNF
import BNFC.Backend.Haskell.Utils
import BNFC.Backend.Txt2Tag
import BNFC.Backend.XML
import qualified BNFC.Backend.Common.Makefile as Makefile

import BNFC.CF
import BNFC.Options hiding (Backend)
import BNFC.Utils (when, unless, table, getZonedTimeTruncatedToSeconds)


-- | Entrypoint for the Haskell backend.

makeHaskell :: SharedOptions -> CF -> Backend
makeHaskell opts cf = do
  -- Get current time in printable form.
  time <- liftIO $ show <$> getZonedTimeTruncatedToSeconds

  let absMod = absFileM opts
      lexMod = alexFileM opts
      parMod = happyFileM opts
      prMod  = printerFileM opts
      layMod = layoutFileM opts
      errMod = errFileM opts
      shareMod = shareFileM opts
  do
    -- Generate abstract syntax and pretty printer.
    mkfile (absFile opts) $ cf2Abstract (tokenText opts) (generic opts) (functor opts) absMod cf
    mkfile (printerFile opts) $ cf2Printer (tokenText opts) (functor opts) False prMod absMod cf

    -- Generate Alex lexer.  Layout is resolved after lexing.
    case alexMode opts of
      Alex1 -> do
        mkfile (alexFile opts) $ cf2alex lexMod cf
        liftIO $ printf "Use Alex 1.1 to compile %s.\n" (alexFile opts)
      Alex2 -> do
        mkfile (alexFile opts) $ cf2alex2 lexMod shareMod (shareStrings opts) (tokenText opts) cf
        liftIO $ printf "Use Alex 2.0 to compile %s.\n" (alexFile opts)
      Alex3 -> do
        mkfile (alexFile opts) $ cf2alex3 lexMod shareMod (shareStrings opts) (tokenText opts) cf
        liftIO $ printf "Use Alex 3.0 to compile %s.\n" (alexFile opts)

    Ctrl.when (shareStrings opts) $ mkfile (shareFile opts) $ sharedString shareMod (tokenText opts) cf
    Ctrl.when (hasLayout cf) $ mkfile (layoutFile opts) $
      cf2Layout (tokenText opts) (alex1 opts) layMod lexMod cf

    -- Generate Happy parser and matching test program unless --cnf.
    Ctrl.unless (cnf opts) $ do
      mkfile (happyFile opts) $
        cf2Happy parMod absMod lexMod (glr opts) (tokenText opts) (functor opts) cf
      -- liftIO $ printf "%s Tested with Happy 1.15\n" (happyFile opts)
      mkfile (tFile opts)        $ testfile opts cf

    -- Both Happy parser and skeleton (template) rely on Err.
    mkfile (errFile opts) $ mkErrM errMod
    mkfile (templateFile opts) $ cf2Template (templateFileM opts) absMod (functor opts) cf

    -- Generate txt2tags documentation.
    mkfile (txtFile opts)      $ cfToTxt (lang opts) cf

    -- Generate XML and DTD printers.
    case xml opts of
      2 -> makeXML opts True cf
      1 -> makeXML opts False cf
      _ -> return ()

    -- CNF backend.  Currently does not make use of layout.
    Ctrl.when (cnf opts) $ do
      mkfile (cnfTablesFile opts) $ ToCNF.generate opts cf
      mkfile (cnfTestFile opts)   $ ToCNF.genTestFile opts cf
      mkfile (cnfBenchFile opts)  $ ToCNF.genBenchmark opts

    -- Generate Agda bindings for AST, Printer and Parser.
    Ctrl.when (agda opts) $ makeAgda time opts cf

    -- Generate Makefile.
    Makefile.mkMakefile opts $ makefile opts


-- | Generate the makefile (old version, with just one "all" target).
_oldMakefile
  :: Options
  -> String    -- ^ Filename of the makefile.
  -> Doc       -- ^ Content of the makefile.
_oldMakefile opts makeFile = vcat
  [ Makefile.mkRule "all" [] $ concat $
      [ [ unwords $ [ "happy -gca" ] ++ glrParams ++ [ happyFile opts ] | not (cnf opts) ]
      , [ "alex -g " ++ alexFile opts ]
      , [ if cnf opts
          then unwords [ "ghc --make", cnfTestFile opts, "-o", cnfTestFileExe opts ]
          else unwords [ "ghc --make", tFile       opts, "-o", tFileExe       opts ]
        ]
      ]
  , cleanRule opts
  , distCleanRule opts makeFile
  ]
  where
  glrParams :: [String]
  glrParams = when (glr opts == GLR) $ [ "--glr", "--decode" ]

-- | Rule to clean GHC and Latex generated files.
cleanRule :: Options -> Doc
cleanRule opts = Makefile.mkRule "clean" [] $ concat $
  [ [ rmGen ]
  , when (agda opts) rmAgda
  ]
  where
  rmGen  = unwords $ [ "-rm", "-f" ] ++ map prefix gen
  gen    = concat [ genHs, genLtx, genAg ]
  genHs  = [ "*.hi", "*.o" ]
  genLtx = [ "*.log", "*.aux", "*.dvi" ]
  genAg  = when (agda opts) $ [ "*.agdai" ]
  rmAgda = [ "-rm -rf MAlonzo" ]
  prefix = if null dir then id else (dir </>)
  dir    = codeDir opts

-- | Rule to clean all files generated by BNFC and the subsequent tools.
distCleanRule :: Options -> String -> Doc
distCleanRule opts makeFile = Makefile.mkRule "distclean" ["clean"] $
  [ unwords . concat $
    [ [ "-rm -f" ]
      -- Generated files that have a .bak variant
    , concatMap (\ f -> alsoBak (f opts))
      [ absFile        -- Abs.hs
      , composOpFile   -- ComposOp.hs
      , txtFile        -- Doc.txt
      , errFile        -- ErrM.hs
      , layoutFile     -- Layout.hs
      , alexFile       -- Lex.x
      , happyFile      -- Par.y
      , printerFile    -- Print.hs
      , shareFile      -- SharedString.hs -- only if: shareStrings opt
      , templateFile   -- Skel.hs
      , tFile          -- Test.hs
      , xmlFile        -- XML.hs
      , agdaASTFile    -- AST.agda
      , agdaParserFile -- Parser.agda
      , agdaLibFile    -- IOLib.agda
      , agdaMainFile   -- Main.agda
      , (\ opts -> dir ++ lang opts ++ ".dtd")
      ]  -- TODO: clean up cnf files
      -- Files that have no .bak variant
    , map (\ (file, ext) -> mkFile withLang file ext opts)
      [ ("Test"    , "")
      , ("Lex"     , "hs")
      , ("Par"     , "hs")
      , ("Par"     , "info")
      , ("ParData" , "hs")  -- only if --glr
      ]
    , [ "Main" | agda opts ]
    , [ makeFile ]
    ]
  , if null dir then "" else "-rmdir -p " ++ dir
  ]
  where
  dir = let d = codeDir opts in if null d then "" else d ++ [pathSeparator]

  alsoBak :: FilePath -> [FilePath]
  alsoBak s = [ s, s <.> "bak" ]

makefileHeader :: Doc
makefileHeader = vcat
  [ "# Makefile generated by BNFC."
  , ""
  ]


-- | Generate the makefile.
makefile
  :: Options
  -> String    -- ^ Filename of the makefile.
  -> Doc       -- ^ Content of the makefile.
makefile opts makeFile = vcat
  [ makefileHeader
  , phonyRule
  , defaultRule
  , vcat [ "# Rules for building the parser." , "" ]
  -- If option -o was given, we have no access to the grammar file
  -- from the Makefile.  Thus, we have to drop the rule for
  -- reinvokation of bnfc.
  , when (isDefault outDir opts) $ bnfcRule
  , unless (cnf opts) $ happyRule
  , alexRule
  , if cnf opts then testCNFRule else testParserRule
  , when (agda opts) $ agdaRule
  , vcat [ "# Rules for cleaning generated files." , "" ]
  , cleanRule opts
  , distCleanRule opts makeFile
  , "# EOF"
  ]
  where
  -- | List non-file targets here.
  phonyRule :: Doc
  phonyRule = vcat
    [ "# List of goals not corresponding to file names."
    , ""
    , Makefile.mkRule ".PHONY" [ "all", "clean", "distclean" ] []
    ]
  -- | Default: build test parser(s).
  defaultRule :: Doc
  defaultRule = vcat
     [ "# Default goal."
     , ""
     , Makefile.mkRule "all" tgts []
     ]
     where
     tgts | cnf opts  = [ cnfTestFileExe opts ]
          | otherwise = concat $
              [ [ tFileExe opts ]
              , [ "Main" | agda opts ]
              ]

  -- | Rule to reinvoke @bnfc@ to updated parser.
  --   Reinvokation should not recreate @Makefile@!
  bnfcRule :: Doc
  bnfcRule = Makefile.mkRule tgts [ lbnfFile opts ] [ recipe ]
    where
    recipe    = unwords [ "bnfc", printOptions opts{ make = Nothing } ]
    tgts      = unwords . concat $
      [ alexEtc
      , if cnf opts then [ cnfTestFile opts ] else [ happyFile opts, tFile opts ]
      , when (agda opts) agdaFiles
      ]
    alexEtc   = map ($ opts) [ errFile, alexFile, printerFile ]
    agdaFiles = map ($ opts) [ agdaASTFile, agdaParserFile, agdaLibFile, agdaMainFile ]

  -- | Rule to invoke @happy@.
  happyRule :: Doc
  happyRule = Makefile.mkRule "%.hs" [ "%.y" ] [ recipe ]
    where
    recipe = unwords . concat $
      [ [ "happy", "--ghc", "--coerce", "--array", "--info" ]
      , when (glr opts == GLR) $ [ "--glr", "--decode" ]
      , [ "$<" ]
      ]

  -- | Rule to invoke @alex@.
  alexRule :: Doc
  alexRule = Makefile.mkRule "%.hs" [ "%.x" ] [ "alex --ghc $<" ]

  -- | Rule to build Haskell test parser.
  testParserRule :: Doc
  testParserRule = Makefile.mkRule tgt deps [ "ghc --make $< -o $@" ]
    where
    tgt :: String
    tgt = tFileExe opts
    deps :: [String]
    deps = map ($ opts)
      [ tFile {- must be first! -}
      , errFile
      , alexFileHs
      , happyFileHs
      , printerFile
      ]

  -- | Rule to build CNF test parser.
  testCNFRule :: Doc
  testCNFRule = Makefile.mkRule (cnfTestFileExe opts) deps [ "ghc --make $< -o $@" ]
    where
    deps = [ cnfTestFile opts {- must be first! -} , alexFileHs opts ]

  -- | Rule to build Agda parser.
  agdaRule :: Doc
  agdaRule = Makefile.mkRule "Main" deps [ "agda --no-libraries --ghc --ghc-flag=-Wwarn $<" ]
    where
    deps = map ($ opts)
      [ agdaMainFile  -- must be first!
      , agdaASTFile
      , agdaParserFile
      , agdaLibFile
      -- Haskell modules bound by Agda modules:
      , errFile
      , alexFileHs
      , happyFileHs
      , printerFile
      ]

testfile :: Options -> CF -> String
testfile opts cf = unlines $ concat $
  [ [ "-- Program to test parser, automatically generated by BNF Converter."
    , ""
    , "module Main where"
    , ""
    ]
  , case tokenText opts of
      StringToken -> []
      TextToken ->
        [ "import Prelude hiding ( getContents, readFile )"
        , "import Data.Text.IO   ( getContents, readFile )"
        , "import qualified Data.Text"
        ]
      ByteStringToken ->
        [ "import Prelude hiding        ( getContents, readFile )"
        , "import Data.ByteString.Char8 ( getContents, readFile )"
        , "import qualified Data.ByteString.Char8 as BS"
        ]
  , [ "import System.Environment ( getArgs, getProgName )"
    , "import System.Exit        ( exitFailure, exitSuccess )"
    , "import Control.Monad      ( when )"
    , ""
    ]
  , table "" $ concat
    [ [ [ "import " , alexFileM     opts , " ( Token )"                          ]
      , [ "import " , happyFileM    opts , " ( " ++ firstParser ++ ", myLexer )" ]
      , [ "import " , templateFileM opts , " ()"                                 ]
      , [ "import " , printerFileM  opts , " ( Print, printTree )"               ]
      , [ "import " , absFileM      opts , " ()"                                 ]
      ]
    , [ [ "import " , layoutFileM   opts , " ( resolveLayout )" ] | lay     ]
    , [ [ "import " , xmlFileM      opts , " ( XPrint, printXML )"      ] | use_xml ]
    ]
  , [ "import qualified Data.Map ( Map, lookup, toList )" | use_glr ]
  , [ "import Data.Maybe ( fromJust )"                    | use_glr ]
  , [ ""
    , "type Err = Either String"
    , if use_glr
      then "type ParseFun a = [[Token]] -> (GLRResult, GLR_Output (Err a))"
      else "type ParseFun a = [Token] -> Err a"
    , ""
    , "myLLexer = " ++ if lay then "resolveLayout True . myLexer"
                              else "myLexer"
    , ""
    , "type Verbosity = Int"
    , ""
    , "putStrV :: Verbosity -> String -> IO ()"
    , "putStrV v s = when (v > 1) $ putStrLn s"
    , ""
    , "runFile :: (" ++ xpr ++ if_glr "TreeDecode a, " ++ "Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()"
    , "runFile v p f = putStrLn f >> readFile f >>= run v p"
    , ""
    , "run :: (" ++ xpr ++ if_glr "TreeDecode a, " ++ "Print a, Show a) => Verbosity -> ParseFun a -> " ++ tokenTextType (tokenText opts) ++ " -> IO ()"
    , if use_glr then runGlr else runStd use_xml
    , ""
    , "showTree :: (Show a, Print a) => Int -> a -> IO ()"
    , "showTree v tree"
    , " = do"
    , "      putStrV v $ \"\\n[Abstract Syntax]\\n\\n\" ++ show tree"
    , "      putStrV v $ \"\\n[Linearized tree]\\n\\n\" ++ printTree tree"
    , ""
    , "usage :: IO ()"
    , "usage = do"
    , "  putStrLn $ unlines"
    , "    [ \"usage: Call with one of the following argument combinations:\""
    , "    , \"  --help          Display this help message.\""
    , "    , \"  (no arguments)  Parse stdin verbosely.\""
    , "    , \"  (files)         Parse content of files verbosely.\""
    , "    , \"  -s (files)      Silent mode. Parse content of files silently.\""
    , "    ]"
    , "  exitFailure"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "  args <- getArgs"
    , "  case args of"
    , "    [\"--help\"] -> usage"
    , "    [] -> getContents >>= run 2 " ++ firstParser
    , "    \"-s\":fs -> mapM_ (runFile 0 " ++ firstParser ++ ") fs"
    , "    fs -> mapM_ (runFile 2 " ++ firstParser ++ ") fs"
    , ""
    ]
  , if use_glr then
    [ "the_parser :: ParseFun " ++ show topType
    , "the_parser = lift_parser " ++ render (parserName topType)
    , ""
    , liftParser
    ]
    else []
  ]
  where
    lay         = hasLayout cf
    use_xml     = xml opts > 0
    xpr         = if use_xml then "XPrint a, "     else ""
    use_glr     = glr opts == GLR
    if_glr s    = if use_glr then s else ""
    firstParser = if use_glr then "the_parser" else render (parserName topType)
    topType     = firstEntry cf


runStd xml
 = unlines
   [ "run v p s = case p ts of"
   , "    Left s -> do"
   , "      putStrLn \"\\nParse              Failed...\\n\""
   , "      putStrV v \"Tokens:\""
   , "      putStrV v $ show ts"
   , "      putStrLn s"
   , "      exitFailure"
   , "    Right tree -> do"
   , "      putStrLn \"\\nParse Successful!\""
   , "      showTree v tree"
   , if xml then
     "      putStrV v $ \"\\n[XML]\\n\\n\" ++ printXML tree"
     else ""
   , "      exitSuccess"
   , "  where"
   , "  ts = myLLexer s"
   ]

runGlr
 = unlines
   [ "run v p s"
   , " = let ts = map (:[]) $ myLLexer s"
   , "       (raw_output, simple_output) = p ts in"
   , "   case simple_output of"
   , "     GLR_Fail major minor -> do"
   , "                               putStrLn major"
   , "                               putStrV v minor"
   , "     GLR_Result df trees  -> do"
   , "                               putStrLn \"\\nParse Successful!\""
   , "                               case trees of"
   , "                                 []        -> error \"No results but parse succeeded?\""
   , "                                 [Right x] -> showTree v x"
   , "                                 xs@(_:_)  -> showSeveralTrees v xs"
   , "   where"
   , "  showSeveralTrees :: (Print b, Show b) => Int -> [Err b] -> IO ()"
   , "  showSeveralTrees v trees"
   , "   = sequence_ "
   , "     [ do putStrV v (replicate 40 '-')"
   , "          putStrV v $ \"Parse number: \" ++ show n"
   , "          showTree v t"
   , "     | (Right t,n) <- zip trees [1..]"
   , "     ]"
   ]


liftParser
 = unlines
   [ "type Forest = Data.Map.Map ForestId [Branch]      -- omitted in ParX export."
   , "data GLR_Output a"
   , " = GLR_Result { pruned_decode     :: (Forest -> Forest) -> [a]"
   , "              , semantic_result   :: [a]"
   , "              }"
   , " | GLR_Fail   { main_message :: String"
   , "              , extra_info   :: String"
   , "              }"
   , ""
   , "lift_parser"
   , " :: (TreeDecode a, Show a, Print a)"
   , " => ([[Token]] -> GLRResult) -> ParseFun a"
   , "lift_parser parser ts"
   , " = let result = parser ts in"
   , "   (\\o -> (result, o)) $"
   , "   case result of"
   , "     ParseError ts f -> GLR_Fail \"Parse failed, unexpected token(s)\\n\""
   , "                                 (\"Tokens: \" ++ show ts)"
   , "     ParseEOF   f    -> GLR_Fail \"Parse failed, unexpected EOF\\n\""
   , "                                 (\"Partial forest:\\n\""
   , "                                    ++ unlines (map show $ Data.Map.toList f))"
   , "     ParseOK r f     -> let find   f = fromJust . ((flip Data.Map.lookup) f)"
   , "                            dec_fn f = decode (find f) r"
   , "                        in GLR_Result (\\ff -> dec_fn $ ff f) (dec_fn f)"
   ]
