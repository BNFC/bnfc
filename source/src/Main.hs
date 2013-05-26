{-
    BNF Converter: Main file
    Copyright (C) 2002-2013  Authors:
    Jonas Almström Duregård, Krasimir Angelov, Jean-Philippe Bernardy, Björn Bringert, Johan Broberg, Paul Callaghan,
    Grégoire Détrez, Markus Forsberg, Ola Frid, Peter Gammie, Thomas Hallgren, Patrik Jansson,
    Kristofer Johannisson, Antti-Juhani Kaijanaho, Ulf Norell,
    Michael Pellauer and Aarne Ranta 2002 - 2013.

    Björn Bringert, Johan Broberg, Markus Forberg, Peter Gammie,
    Patrik Jansson, Antti-Juhani Kaijanaho, Ulf Norell,
    Michael Pellauer, Aarne Ranta

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


module Main where

-- import Utils
import BNFC.CF (cfp2cf)
import qualified BNFC.Backend.Latex as Latex
import qualified BNFC.Backend.Haskell as Haskell
import BNFC.Backend.HaskellGADT
import BNFC.Backend.HaskellProfile
import BNFC.Backend.Java
import BNFC.Backend.CPP.NoSTL
import BNFC.Backend.CSharp
import BNFC.Backend.CPP.STL
import BNFC.Backend.C
import BNFC.Backend.OCaml
import BNFC.Backend.XML
import BNFC.Utils
import qualified BNFC.Options as O
import BNFC.GetCF

import BNFC.MultiView (preprocessMCF, mkTestMulti, mkMakefileMulti)

import System.Environment (getEnv,getArgs     )
import System.Exit (exitFailure,exitSuccess)
import System.Cmd (system)
import Data.Char
import Data.List (elemIndex, foldl')
import Control.Monad (when,unless)
import Paths_BNFC ( version )
import Data.Version ( showVersion )

import System.FilePath
import System.IO (stderr, hPutStrLn,hPutStr)
import BNFC.Options
import System.Console.GetOpt
-- Print an error message and a (short) usage help and exit
printUsageErrors :: [String] -> IO ()
printUsageErrors msg = do
  mapM_ (hPutStrLn stderr) msg
  hPutStrLn stderr usage
  exitFailure

main :: IO ()
main = do
  args <- getArgs

  -- First, wo look for deprecated options in the arguments
  -- if we find any, we report them and exit immediately
  case lookForDeprecatedOptions args of
    [] -> return ()
    msgs -> printUsageErrors msgs

  case parseMode args of
    -- FIXME As long as we are falling back on the old option parser for
    -- some modes, we cannot trust those error messages
    -- UsageError e -> printUsageErrors [e]
    Help -> putStrLn help >> exitSuccess
    Version ->  putStrLn (showVersion version) >> exitSuccess
    Target TargetLatex args' f ->
      readFile f >>= parseLbnf TargetLatex >>= Latex.backend args' (name f)
    _ -> mkOne args
  where name = takeBaseName



-- next, we parse global options such as --version
--  let bnfcOptions = [
--        Option [] ["help"] (NoArg Help) "show help",
--        Option [] ["version"] (NoArg Version) "show version number",
--        Option [] ["multilingual"] (NoArg Multilingual) "multilingual BNF" ]
--  case getOpt' RequireOrder bnfcOptions args of
--    -- if --version is present, we print the version and exit
--    (Version:_,_,_,_) -> putStrLn (showVersion version) >> exitSuccess
--    -- if --help is present, we print usage and exit
--    (Help:_,_,_,_) -> printUsage >> exitSuccess
--    -- Mystery 'multilingual BNF' preprocessing (doc?)
--    ([Multilingual],_,_,[]) ->
--      do putStrLn "preprocessing multilingual BNF"
--         let file = last args
--         (files,entryp) <- preprocessMCF file
--         mapM_ mkOne [init args ++ [f] | f <- files]
--         mkTestMulti entryp args file files
--         mkMakefileMulti args file files
--    -- LaTeX backend
--    ([],"latex":args',[],[]) -> do
--      Latex.main args'
--    -- standard case
--    ([],_,_,[]) -> mkOne args
--    -- Anything else: print usage message
--    (_,_,_,errs) -> printUsageErrors errs




mkOne :: [String] -> IO ()
mkOne xx =
  case O.parseArguments xx of
    Left err -> printUsageErrors [err ++ "\n"]
    Right (options,file) -> do
      let name = takeWhile (/= '.') $ takeFileName file
      putStrLn title
      (cfp, isOk) <- tryReadCFP (head $ O.targets options ++ [TargetHaskell]) file
      let cf = cfp2cf cfp
      unless isOk $
        fail "Error: Failed"
      case O.targets options of
           [ O.TargetC ]          -> makeC (O.make options) name cf
           [ O.TargetCppNoStl ]   -> makeCPP (O.make options) name cf
           [ O.TargetCpp ]        -> makeSTL (O.make options)
                                             -- FIXME: should be an option
                                             False
                                             (O.inPackage options)
                                             name cf
           [ O.TargetCSharp ]     -> makeCSharp (O.make options)
                                                -- FIXME: should be an option
                                                False
                                                -- FIXME: should be an option
                                                False
                                                (O.inPackage options)  cf file
           [ O.TargetJava ]       -> makeJava15 (O.make options)
                                                (O.inPackage options)  name cf
           [ O.TargetOCaml ]      -> makeOCaml options cf
           [ O.TargetProfile ]    -> makeAllProfile (O.make options)
                                                    (O.alexMode options == O.Alex1)
                                                    False
                                                    (O.xml options)  name cfp
           [ O.TargetHaskellGadt] -> makeAllGADT options cf
           _                      -> Haskell.makeAll options cf
      when (O.make options && O.multi options) $ do
            system ("cp Makefile Makefile_" ++ name)
            return ()
      putStrLn "Done!"
 where isCF ('f':'c':'.':_)     = True
       isCF ('f':'n':'b':'.':_) = True
       isCF ('f':'n':'b':'l':'.':_) = True
       isCF ('c':'f':'n':'b':'.':_) = True
       isCF _                   = False

printUsage :: IO ()
printUsage = do
  putStrLn title
  putStrLn "Usage: bnfc <makeoption>* <language>? <special>* file.cf"
  putStrLn ""
  putStrLn "Version options:"
  putStrLn "  --numeric-version  Print just the version number"
  putStrLn ""
  putStrLn "Make option:"
  putStrLn "  -m             generate Makefile"
  putStrLn ""
  putStrLn "Languages (Only one language mode may be selected.)"
  putStrLn "  -java          Output Java 1.5 code for use with JLex and CUP"
  putStrLn "  -java1.5       Output Java 1.5 code for use with JLex and CUP (same as -java)"
  putStrLn "  -c             Output C code for use with FLex and Bison"
  putStrLn "  -cpp           Output C++ code for use with FLex and Bison (same as -cpp_stl)"
  putStrLn "  -cpp_stl       Output C++ code for use with FLex and Bison (same as -cpp)"
  putStrLn "  -cpp_no_stl    Output C++ code (without STL) for use with FLex and Bison (before 2.5 was: -cpp)"
  putStrLn "                 and the Standard Template Library"
  putStrLn "  -csharp        Output C# code for use with GPLEX and GPPG"
  putStrLn "  -ocaml         Output OCaml code for use with ocamllex and ocamlyacc"
  putStrLn "  -fsharp        Output F# code for use with fslex and fsyacc"
  putStrLn "  -haskell       Output Haskell code for use with Alex and Happy (default)"
  putStrLn "  -prof          Output Haskell code for rules with permutation profiles"
  putStrLn "  -gadt          Output Haskell code which uses GADTs"
  putStrLn ""
  putStrLn "Special options for the Haskell back-end:"
  putStrLn "  -d             Put Haskell code in modules Lang.* instead of Lang*"
  putStrLn "  -p <name>      Prepend <name> to the Haskell module names."
  putStrLn "                 Dots in the module name create hierarchical modules."
  putStrLn "  -alex1         Use Alex 1.1 as Haskell lexer tool"
  putStrLn "  -alex2         Use Alex 2 as Haskell lexer tool"
  putStrLn "  -alex3         Use Alex 3 as Haskell lexer tool (default)"
  putStrLn "  -sharestrings  Use string sharing in Alex 2 lexer"
  putStrLn "  -bytestrings   Use byte string in Alex 2 lexer"
  putStrLn "  -glr           Output Happy GLR parser"
  putStrLn "  -xml           Also generate a DTD and an XML printer"
  putStrLn "  -xmlt          DTD and an XML printer, another encoding"
  putStrLn ""
  putStrLn "Special options for the C++ (with STL) back-end:"
  putStrLn "  -l             Add and set line_number field for all syntax classes"
  putStrLn "  -p <namespace> Use <namespace> as the C++ namespace"
  putStrLn ""
  putStrLn "Special options for the Java (v 1.5) back-end:"
  putStrLn "  -p <package>   Prepend <package> to the Java package name"
  putStrLn ""
  putStrLn "Special options for the C# backend:"
  putStrLn "  -p <namespace> Use <namespace> as the C# namespace"
  putStrLn "  -vs            Generate Visual Studio solution/project files"
  putStrLn "  -wcf           Add support for Windows Communication Foundation, by"
  putStrLn "                 marking abstract syntax classes as DataContracts"
  exitFailure
