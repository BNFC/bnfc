{-
    BNF Converter: Main file
    Copyright (C) 2002-2010  Authors:
    Bj�rn Bringert, Johan Broberg, Markus Forberg, Peter Gammie,
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

import MultiView (preprocessMCF, mkTestMulti, mkMakefileMulti)

import System.Environment
import System.Exit
import System.Process (system)
import Data.Char
import Data.List (elemIndex)

version = "2.4.2.0"

title = unlines [
  "The BNF Converter, "++version,
  "(c) Krasimir Angelov, Bjorn Bringert, Johan Broberg, Paul Callaghan, ",
  "    Markus Forsberg, Ola Frid, Peter Gammie, Patrik Jansson, ",
  "    Kristofer Johannisson, Antti-Juhani Kaijanaho, Ulf Norell, ",
  "    Michael Pellauer and Aarne Ranta 2002 - 2010.",
  "Free software under GNU General Public License (GPL).",
  "Bug reports to {markus,aarne}@cs.chalmers.se."
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
      let name = takeWhile (/= '.') $ basename file
      let make = elem "-m" args
      let multi = elem "-multi" args
      let c = elem "-c" args
      let cpp = elem "-cpp" args
      let cpp_stl = elem "-cpp_stl" args
      let csharp = elem "-csharp" args
      let java = elem "-java" args
      let java15 = elem "-java1.5" args
      let ocaml = elem "-ocaml" args
      let fsharp = elem "-fsharp" args
      let haskell = elem "-haskell" args
      let haskellGADT = elem "-gadt" args
      let profile = elem "-prof" args
      let alex1 = "-alex1" `elem` args
          alex2 = "-alex2" `elem` args
	  alex1or2 = alex1
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
      if checkUsage False [c, cpp, cpp_stl, csharp, java, haskell, profile] then
       do
       if (isCF (reverse file)) then
        do
         putStrLn title
         case () of
           _ | c      -> makeC make name file
           _ | cpp    -> makeCPP make name file
           _ | cpp_stl-> makeSTL make linenumbers inPackage name file
           _ | csharp -> makeCSharp make vsfiles wcfSupport inPackage file
           _ | java   -> makeJava make name file
           _ | java15 -> makeJava15 make inPackage name file
           _ | ocaml  -> makeOCaml make alex1or2 inDir alex2StringSharing glr xml inPackage name file
           _ | fsharp -> makeFSharp make alex1or2 inDir alex2StringSharing glr xml inPackage name file
           _ | profile-> makeAllProfile make alex1or2 False xml name file
	   _ | haskellGADT -> makeAllGADT make alex1or2 inDir alex2StringSharing alex2ByteString glr xml inPackage name file
           _          -> makeAll make alex1or2 inDir alex2StringSharing alex2ByteString glr xml inPackage name multi file
         if (make && multi)
            then (system ("cp Makefile Makefile_" ++ name)) >> return ()
            else return ()
	else endFileErr
       else endLanguageErr
 where isCF ('f':'c':'.':_) = True
       isCF _               = False
       endFileErr = do
                      putStr title
                      putStrLn "Error: the input file must end with .cf"
		      exitFailure
       endLanguageErr = do
                          putStr title
                          putStrLn "Error: only one language mode may be chosen"
			  exitFailure

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
  putStrLn "  -java          Output Java code for use with JLex and CUP"
  putStrLn "  -java1.5       Output Java 1.5 code for use with JLex and CUP"
  putStrLn "  -c             Output C code for use with FLex and Bison"
  putStrLn "  -cpp           Output C++ code for use with FLex and Bison"
  putStrLn "  -cpp_stl       Output C++ code for use with FLex and Bison"
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
  putStrLn "  -alex2         Use Alex 2 as Haskell lexer tool (default)"
  putStrLn "  -sharestrings  Use string sharing in Alex 2 lexer"
  putStrLn "  -bytestrings   Use byte string in Alex 2 lexer"
  putStrLn "  -glr           Output Happy GLR parser"
  putStrLn "  -xml           Also generate a DTD and an XML printer"
  putStrLn "  -xmlt          DTD and an XML printer, another encoding"
  putStrLn ""
  putStrLn "Special options for the C++ STL back-end:"
  putStrLn "  -l             Add and set line_number field for all syntax classes"
  putStrLn "  -p <namespace> Use <namespace> as the C++ namespace"
  putStrLn ""
  putStrLn "Special options for the Java 1.5 back-end:"
  putStrLn "  -p <package>   Prepend <package> to the Java package name"
  putStrLn ""
  putStrLn "Special options for the C# backend:"
  putStrLn "  -p <namespace> Use <namespace> as the C# namespace"
  putStrLn "  -vs            Generate Visual Studio solution/project files"
  putStrLn "  -wcf           Add support for Windows Communication Foundation, by"
  putStrLn "                 marking abstract syntax classes as DataContracts"
  exitFailure
--		      putStrLn "          : -gf        write GF files"


checkUsage _ [] = True
checkUsage True (True:xs) = False
checkUsage False (True:xs) = checkUsage True xs
checkUsage old (x:xs) = checkUsage old xs
