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
    Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1335, USA
-}


module Main where

import BNFC.Backend.Base hiding (Backend)
import BNFC.Backend.C
import BNFC.Backend.CPP.NoSTL
import BNFC.Backend.CPP.STL
import BNFC.Backend.CSharp
import BNFC.Backend.Haskell
import BNFC.Backend.HaskellGADT
import BNFC.Backend.HaskellProfile
import BNFC.Backend.Java
import BNFC.Backend.Latex
import BNFC.Backend.OCaml
import BNFC.Backend.Pygments
import BNFC.GetCF
import BNFC.Options hiding (make)

import Paths_BNFC ( version )

import Data.Version ( showVersion )
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr, hPutStrLn)

-- Print an error message and a (short) usage help and exit
printUsageErrors :: [String] -> IO ()
printUsageErrors msg = do
  mapM_ (hPutStrLn stderr) msg
  hPutStrLn stderr usage
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case parseMode args of
    UsageError e -> printUsageErrors [e]
    Help    -> putStrLn help >> exitSuccess
    Version ->  putStrLn (showVersion version) >> exitSuccess
    Target options file | target options == TargetProfile ->
      readFile file >>= parseCFP options TargetProfile
                    >>= writeFiles (outDir options) . makeHaskellProfile options
    Target options file ->
      readFile file >>= parseCF options (target options) >>= make (target options) options
  where
    make t opts cf = writeFiles (outDir opts) $ (maketarget t) opts cf

maketarget t = case t of
    TargetC            -> makeC
    TargetCpp          -> makeCppStl
    TargetCppNoStl     -> makeCppNoStl
    TargetCSharp       -> makeCSharp
    TargetHaskell      -> makeHaskell
    TargetHaskellGadt  -> makeHaskellGadt
    TargetLatex        -> makeLatex
    TargetJava         -> makeJava
    TargetOCaml        -> makeOCaml
    TargetProfile      -> error "Not implemented"
    TargetPygments     -> makePygments
