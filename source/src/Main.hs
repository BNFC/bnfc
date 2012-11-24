{-
    BNF Converter: Main file
    Copyright (C) 2002-2010  Authors: 
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
-- import CF
import HaskellTop
import HaskellTopGADT
import ProfileTop
import JavaTop
import JavaTop15
import CPPTop
import CSharpTop
import STLTop
import CTop
import OCamlTop
import FSharpTop
import CFtoXML
import Utils
import Options
import GetCF

import MultiView (preprocessMCF, mkTestMulti, mkMakefileMulti)

import System.Environment (getEnv,getArgs     )
import System.Exit (exitFailure,exitSuccess)
import System.Cmd (system)
import Data.Char
import Data.List (elemIndex, foldl')
import Control.Monad (when,unless)

version = "2.6a"

title = unlines [
  "The BNF Converter, "++version, 
  "(c) Krasimir Angelov, Jean-Philippe Bernardy, Bjorn Bringert, Johan Broberg, Paul Callaghan, ",
  "    Markus Forsberg, Ola Frid, Peter Gammie, Patrik Jansson, ",
  "    Kristofer Johannisson, Antti-Juhani Kaijanaho, Ulf Norell, ",
  "    Michael Pellauer and Aarne Ranta 2002 - 2012.",
  "Free software under GNU General Public License (GPL).",
  "Bug reports to bnfc-dev@googlegroups.com."
 ]
 
main :: IO ()
main = do
  xx <- getArgs
	  
  case xx of
    ["--numeric-version"] -> do
      putStrLn version
      exitSuccess
    [] -> printUsage
    _ | elem "-multi" xx -> do
      putStrLn "preprocessing multilingual BNF"
      let file = last xx
      (files,entryp) <- preprocessMCF file
      mapM_ mkOne [init xx ++ [f] | f <- files]
      mkTestMulti entryp xx file files
      mkMakefileMulti xx file files
    _ -> mkOne xx

mkOne :: [String] -> IO ()
mkOne xx = do   
  let args = (map (filter (not . isSpace)) xx)
  let file = last args  
  if (head file == '-') then printUsage 
   else do
      let name = takeWhile (/= '.') $ basename file
      let make = elem "-m" args
      let multi = elem "-multi" args
      let c = elem "-c" args
      let cpp_no_stl = elem "-cpp_no_stl" args 
      let cpp_stl = elem "-cpp_stl" args || elem "-cpp" args
      let csharp = elem "-csharp" args
      let java14 = elem "-java1.4" args
      let java15 = elem "-java1.5" args || elem "-java" args
      let ocaml = elem "-ocaml" args
      let fsharp = elem "-fsharp" args
      let haskell = elem "-haskell" args
      let haskellGADT = elem "-gadt" args
      let profile = elem "-prof" args
      let alexMode = foldl' (\m arg -> 
                              case arg of
                                "-alex1" -> Alex1
                                "-alex2" -> Alex2
                                "-alex3" -> Alex3
                                _        -> m
                            ) Alex3 args
          alex1 = alexMode == Alex1
          alex2StringSharing = elem "-sharestrings" args
          alex2ByteString    = elem "-bytestrings" args
          glr = "-glr" `elem` args
      let xml = if elem "-xml"  args then 1 else 
                if elem "-xmlt" args then 2 else 0
      let inDir = elem "-d" args
      let vsfiles = elem "-vs" args
      let wcfSupport = elem "-wcf" args
      let linenumbers = elem "-l" args -- for C++ STL target
      inPackage <- case elemIndex "-p" args of
		         Nothing -> return Nothing
			 Just i | i < length args - 1 -> return (Just (args!!(i+1)))
			 _ -> do
			      putStrLn "-p option requires an argument"
			      printUsage
      let options = Options {make = make, 
                             alexMode = alexMode, 
                             inDir = inDir, 
                             shareStrings = alex2StringSharing, 
                             byteStrings = alex2ByteString,
                             glr = if glr then GLR else Standard,
                             xml = xml,
                             inPackage = inPackage,
                             lang = name,
                             multi = multi,
                             cnf = elem "-cnf" args
                             }
          readOptions0 = [ FormatOptC |c] ++ [ FormatOptCPP | cpp_no_stl ] ++ [FormatOptCPP_STL  |  cpp_stl 
                ] ++ [ FormatOptCSharp | csharp] ++ [ FormatOptFSharp |fsharp] ++ [FormatOptHaskellGADT|haskellGADT
                ] ++ [ FormatOptJava15 |java15] ++ [FormatOptJava |java14] ++ [FormatOptOCAML |ocaml] ++ [FormatOptProfile|profile]
          readOptions = if null readOptions0 then [FormatOptHaskell] else readOptions0
      putStrLn title
      unless (length readOptions == 1) $
        fail "Error: only one language mode may be chosen"
      unless (isCF (reverse file)) $ 
        fail "Error: the input file must end with .cf"
      case () of
           _ | c      -> makeC make name file
           _ | cpp_no_stl    -> makeCPP make name file
           _ | cpp_stl-> makeSTL make linenumbers inPackage name file
           _ | csharp -> makeCSharp make vsfiles wcfSupport inPackage file
           _ | java14 -> makeJava make name file
           _ | java15 -> makeJava15 make inPackage name file
           _ | ocaml  -> makeOCaml options file
           _ | fsharp -> makeFSharp options file
           _ | profile-> makeAllProfile make alex1 False xml name file
           _ | haskellGADT -> makeAllGADT options file
           _  -> makeAll options file
      when (make && multi) $ do
            system ("cp Makefile Makefile_" ++ name)
            return ()
 where isCF ('f':'c':'.':_)     = True
       isCF ('f':'n':'b':'.':_) = True
       isCF ('f':'n':'b':'l':'.':_) = True
       isCF ('c':'f':'n':'b':'.':_) = True
       isCF _                   = False
       
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
