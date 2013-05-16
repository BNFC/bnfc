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
import BNFC.Backend.Haskell
import BNFC.Backend.HaskellGADT
import BNFC.Backend.HaskellProfile
import JavaTop
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
import System.FilePath (takeFileName)
import System.IO (stderr, hPutStrLn,hPutStr)
import BNFC.Options (lookForDeprecatedOptions)
import System.Console.GetOpt

title = unlines [
  "The BNF Converter, "++showVersion version,
  "(c) Jonas Almström Duregård, Krasimir Angelov, Jean-Philippe Bernardy, Björn Bringert, Johan Broberg, Paul Callaghan, ",
  "    Grégoire Détrez, Markus Forsberg, Ola Frid, Peter Gammie, Thomas Hallgren, Patrik Jansson, ",
  "    Kristofer Johannisson, Antti-Juhani Kaijanaho, Ulf Norell, ",
  "    Michael Pellauer and Aarne Ranta 2002 - 2013.",
  "Free software under GNU General Public License (GPL).",
  "Bug reports to bnfc-dev@googlegroups.com."
 ]

data Flags = Version | Multilingual

-- Print erre message and a (short) usage help and exit
-- note that the argument is a list of error messages like
-- those returned from getOpt and are expected to contain newline
-- characters already.
printUsageErrors :: [String] -> IO ()
printUsageErrors msgs = do
  mapM_ (hPutStr stderr) msgs
  hPutStrLn stderr "usage: bnfc [--version] [-m] <language> <args> file.cf"
  exitFailure

main :: IO ()
main = do
  args <- getArgs

  -- First, wo look for deprecated options in the arguments
  -- if we find any, we report them and exit immediately
  case lookForDeprecatedOptions args of
    [] -> return ()
    msgs -> printUsageErrors msgs

  -- next, we parse global options such as --version
  let bnfcOptions = [
          Option [] ["version"] (NoArg Version) "show version number"
        , Option [] ["multilingual"] (NoArg Multilingual) "multilingual BNF" ]
  case getOpt' RequireOrder bnfcOptions args of
    -- if --version is present, we print the version and exit
    ([Version],_,_,_) -> putStrLn (showVersion version) >> exitSuccess
    -- Mystery 'multilingual BNF' preprocessing (doc?)
    ([Multilingual],_,_,[]) ->
      do putStrLn "preprocessing multilingual BNF"
         let file = last args
         (files,entryp) <- preprocessMCF file
         mapM_ mkOne [init args ++ [f] | f <- files]
         mkTestMulti entryp args file files
         mkMakefileMulti args file files
    -- standard case
    ([],_,_,[]) -> mkOne args
    -- Anything else: print usage message
    (_,_,_,errs) -> printUsageErrors errs

mkOne :: [String] -> IO ()
mkOne xx =
  case O.parseArguments xx of
    Left err -> printUsageErrors [err ++ "\n"]
    Right (options,file) -> do
      let name = takeWhile (/= '.') $ takeFileName file
      putStrLn title
      (cfp, isOk) <- tryReadCFP options file
      let cf = cfp2cf cfp
      unless isOk $
        fail "Error: Failed"
      case O.targets options of
           [ O.TargetC ]          -> makeC (O.make options) name cf
           [ O.TargetCPP ]        -> makeCPP (O.make options) name cf
           [ O.TargetCPP_STL ]    -> makeSTL (O.make options)
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
           [ O.TargetJava ]       -> makeJava (O.make options)  name cf
           [ O.TargetJava15 ]     -> makeJava15 (O.make options)
                                                (O.inPackage options)  name cf
           [ O.TargetOCAML ]      -> makeOCaml options cf
           [ O.TargetProfile ]    -> makeAllProfile (O.make options)
                                                    (O.alexMode options == O.Alex1)
                                                    False
                                                    (O.xml options)  name cfp
           [ O.TargetHaskellGADT] -> makeAllGADT options cf
           _                      -> makeAll options cf
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
  putStrLn "  -java1.4       Output Java 1.4 code for use with JLex and CUP (before 2.5 was: -java)"
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
