{-
    BNF Converter: C# Main file
    Copyright (C) 2006-2007  Author:  Johan Broberg

    Modified from STLTop 2006.

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
{- 
   **************************************************************
    BNF Converter Module

    Description   : C# Main file

    Author        : Johan Broberg (johan@pontemonti.com)

    License       : GPL (GNU General Public License)

    Created       : 20 November, 2006

    Modified      : 8 January, 2007 by Johan Broberg
   
   ************************************************************** 
-}

module CSharpTop (makeCSharp) where

import Utils
import CF
import OOAbstract
import CAbstoCSharpAbs
import CFtoGPLEX
import CFtoGPPG
import CAbstoCSharpVisitSkeleton
import CAbstoCSharpAbstractVisitSkeleton
import CFtoCSharpPrinter
import CFtoLatex
import CSharpUtils
import Data.Char
import System.Exit (exitFailure)
import System.Environment (getEnv)
import System.Directory
import System.IO
import System.IO.Error (catchIOError)
import System.Process
import Data.Maybe
import Data.Char
import Control.Monad.ST
import qualified BNFC.Backend.Common.Makefile as Makefile
import System.FilePath ((<.>),takeFileName)
-- Control.Monad.State

makeCSharp :: Bool -- Makefile
           -> Bool -- Visual Studio files
           -> Bool -- Windows Communication Foundation support
           -> Maybe Namespace -- C# namespace to use
           -> CF 
           -> FilePath
           -> IO ()
makeCSharp make vsfiles wcfSupport maybenamespace cf file = do
      let namespace    = fromMaybe (filepathtonamespace file) maybenamespace
          cabs         = cf2cabs cf
          absyn        = cabs2csharpabs namespace cabs wcfSupport
          (gplex, env) = cf2gplex namespace cf
          gppg         = cf2gppg namespace cf env
          skeleton     = cabs2csharpvisitskeleton namespace cabs
          absSkeleton  = cabs2csharpAbstractVisitSkeleton namespace cabs
          printer      = cf2csharpprinter namespace cf
          latex        = cfToLatex namespace cf
      writeFileRep "Absyn.cs" absyn
      writeFileRep (namespace ++ ".l") gplex
      putStrLn "   (Tested with GPLEX RC1)"
      writeFileRep (namespace ++ ".y") gppg
      putStrLn "   (Tested with GPPG 1.0)"
      writeFileRep "AbstractVisitSkeleton.cs" absSkeleton
      writeFileRep "VisitSkeleton.cs" skeleton
      writeFileRep "Printer.cs" printer
      writeFileRep "Test.cs" (csharptest namespace cf)
      writeFileRep (namespace ++ ".tex") latex
      if vsfiles then (writeVisualStudioFiles namespace) else return ()
      if make then (writeMakefile namespace) else return ()

writeMakefile :: Namespace -> IO ()
writeMakefile namespace = do 
  writeFileRep "Makefile" makefile
  putStrLn ""
  putStrLn "-----------------------------------------------------------------------------"
  putStrLn "Generated Makefile, which uses mono. You may want to modify the paths to"
  putStrLn "GPLEX and GPPG - unless you are sure that they are globally accessible (the"
  putStrLn "default commands are \"mono gplex.exe\" and \"mono gppg.exe\", respectively."
  putStrLn "The Makefile assumes that ShiftReduceParser.dll is located in ./bin and that"
  putStrLn "is also where test.exe will be generated."
  putStrLn "-----------------------------------------------------------------------------"
  putStrLn ""
  where
    makefile = 
      (unlines [ "MONO = mono", "MONOC = gmcs"
               , "MONOCFLAGS = -optimize -reference:${PARSERREF}"
               , "GPLEX = ${MONO} gplex.exe", "GPPG = ${MONO} gppg.exe"
               , "PARSERREF = bin/ShiftReduceParser.dll"
               -- Apparently GPLEX outputs filenames in 
               -- lowercase, so scanner.cs is supposed to be like that!
              , "CSFILES = Absyn.cs Parser.cs Printer.cs scanner.cs Test.cs VisitSkeleton.cs" ] ++)
      $ Makefile.mkRule "all" [ "test" ]
        []
      $ Makefile.mkRule "clean" []
        -- peteg: don't nuke what we generated - move that to the "vclean" target.
        [ "rm -f " ++ namespace ++ ".pdf test" ]
      $ Makefile.mkRule "distclean" [ "clean" ]
        [ "rm -f ${CSFILES}"
        , "rm -f " ++ unwords [namespace <.> ext | ext <- [ "l","y","tex" ]]
        , "rm -f Makefile" ]
      $ Makefile.mkRule "test" [ "Parser.cs", "Scanner.cs" ]
        [ "@echo \"Compiling test...\""
        , "${MONOC} ${MONOCFLAGS} -out:bin/test.exe ${CSFILES}" ]
      $ Makefile.mkRule "Scanner.cs" [ namespace <.> "l" ]
        [ "${GPLEX} /out:Scanner.cs " ++ namespace <.> "l" ]
      $ Makefile.mkRule "Parser.cs" [ namespace <.> "y" ]
        [ "${GPPG} /gplex " ++ namespace <.> "y > Parser.cs" ]
      $ Makefile.mkDoc namespace
      ""

writeVisualStudioFiles :: Namespace -> IO ()
writeVisualStudioFiles namespace = do
  guid <- projectguid
  writeFileRep (namespace ++ ".csproj") (csproj guid)
  writeFileRep (namespace ++ ".sln") (sln guid)
  writeFileRep "run-gp.bat" batchfile
  putStrLn ""
  putStrLn "-----------------------------------------------------------------------------"
  putStrLn "Visual Studio solution (.sln) and project (.csproj) files were written."
  putStrLn "The project file has a reference to GPLEX/GPPG's ShiftReduceParser. You will"
  putStrLn "have to either copy this file to bin\\ShiftReduceParser.dll or change the"
  putStrLn "reference so that it points to the right location (you can do this from"
  putStrLn "within Visual Studio)."
  putStrLn "Additionally, the project includes Parser.cs and Scanner.cs. These have not"
  putStrLn "been generated yet. You can use the run-gp.bat file to generate them, but"
  putStrLn "note that it requires gppg and gplex to be in your PATH."
  putStrLn "-----------------------------------------------------------------------------"
  putStrLn ""
  where
    batchfile = unlines [
      "@echo off",
      "gppg /gplex " ++ namespace ++ ".y > Parser.cs",
      "gplex /verbose /out:Scanner.cs " ++ namespace ++ ".l"
      ]
    sln guid = unlines [
      "Microsoft Visual Studio Solution File, Format Version 9.00",
      "# Visual Studio 2005",
      "Project(\"{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}\") = \"" ++ namespace ++ "\", \"" ++ namespace ++ ".csproj\", \"" ++ guid ++ "\"",
      "EndProject",
      "Global",
      "  GlobalSection(SolutionConfigurationPlatforms) = preSolution",
      "    Debug|Any CPU = Debug|Any CPU",
      "    Release|Any CPU = Release|Any CPU",
      "  EndGlobalSection",
      "  GlobalSection(ProjectConfigurationPlatforms) = postSolution",
      "    " ++ guid ++ ".Debug|Any CPU.ActiveCfg = Debug|Any CPU",
      "    " ++ guid ++ ".Debug|Any CPU.Build.0 = Debug|Any CPU",
      "    " ++ guid ++ ".Release|Any CPU.ActiveCfg = Release|Any CPU",
      "    " ++ guid ++ ".Release|Any CPU.Build.0 = Release|Any CPU",
      "  EndGlobalSection",
      "  GlobalSection(SolutionProperties) = preSolution",
      "    HideSolutionNode = FALSE",
      "  EndGlobalSection",
      "EndGlobal"
      ]
    csproj guid = unlines [
      "<?xml version=\"1.0\" encoding=\"utf-8\"?>",
      "<Project DefaultTargets=\"Build\" xmlns=\"http://schemas.microsoft.com/developer/msbuild/2003\">",
      "  <PropertyGroup>",
      "    <Configuration Condition=\" '$(Configuration)' == '' \">Debug</Configuration>",
      "    <Platform Condition=\" '$(Platform)' == '' \">AnyCPU</Platform>",
      "    <ProductVersion>8.0.50727</ProductVersion>",
      "    <SchemaVersion>2.0</SchemaVersion>",
      "    <ProjectGuid>" ++ guid ++ "</ProjectGuid>",
      "    <OutputType>Library</OutputType>",
      "    <AppDesignerFolder>Properties</AppDesignerFolder>",
      "    <RootNamespace>" ++ namespace ++ "</RootNamespace>",
      "    <AssemblyName>" ++ namespace ++ "</AssemblyName>",
      "    <StartupObject>",
      "    </StartupObject>",
      "  </PropertyGroup>",
      "  <PropertyGroup Condition=\" '$(Configuration)|$(Platform)' == 'Debug|AnyCPU' \">",
      "    <DebugSymbols>true</DebugSymbols>",
      "    <DebugType>full</DebugType>",
      "    <Optimize>false</Optimize>",
      "    <OutputPath>bin\\Debug\\</OutputPath>",
      "    <DefineConstants>DEBUG;TRACE</DefineConstants>",
      "    <ErrorReport>prompt</ErrorReport>",
      "    <WarningLevel>4</WarningLevel>",
      "  </PropertyGroup>",
      "  <PropertyGroup Condition=\" '$(Configuration)|$(Platform)' == 'Release|AnyCPU' \">",
      "    <DebugType>pdbonly</DebugType>",
      "    <Optimize>true</Optimize>",
      "    <OutputPath>bin\\Release\\</OutputPath>",
      "    <DefineConstants>TRACE</DefineConstants>",
      "    <ErrorReport>prompt</ErrorReport>",
      "    <WarningLevel>4</WarningLevel>",
      "  </PropertyGroup>",
      "  <Import Project=\"$(MSBuildBinPath)\\Microsoft.CSharp.targets\" />",
      "  <!-- To modify your build process, add your task inside one of the targets below and uncomment it. ",
      "       Other similar extension points exist, see Microsoft.Common.targets.",
      "  <Target Name=\"BeforeBuild\">",
      "  </Target>",
      "  <Target Name=\"AfterBuild\">",
      "  </Target>",
      "  -->",
      "  <ItemGroup>",
      "    <Compile Include=\"AbstractVisitSkeleton.cs\" />",
      "    <Compile Include=\"Absyn.cs\" />",
      "    <Compile Include=\"Parser.cs\" />",
      "    <Compile Include=\"Printer.cs\" />",
      "    <Compile Include=\"Scanner.cs\" />",
      "    <Compile Include=\"Test.cs\" />",
      "    <Compile Include=\"VisitSkeleton.cs\" />",
      "  </ItemGroup>",
      "  <ItemGroup>",
      "    <Reference Include=\"ShiftReduceParser, Version=0.0.0.0, Culture=neutral, processorArchitecture=MSIL\">",
      "      <SpecificVersion>False</SpecificVersion>",
      "      <HintPath>bin\\ShiftReduceParser.dll</HintPath>",
      "    </Reference>",
      "  </ItemGroup>",
      "  <ItemGroup>",
      "    <None Include=\"" ++ namespace ++ ".cf\" />",
      "    <None Include=\"" ++ namespace ++ ".l\" />",
      "    <None Include=\"" ++ namespace ++ ".y\" />",
      "    <None Include=\"" ++ namespace ++ ".tex\" />",
      "    <None Include=\"run-gp.bat\" />",
      "  </ItemGroup>",
      "  <PropertyGroup>",
      "    <PreBuildEvent>",
      "    </PreBuildEvent>",
      "  </PropertyGroup>",
      "</Project>"
      ]

csharptest :: Namespace -> CF -> String
csharptest namespace cf = unlines [
  "/*** Compiler Front-End Test automatically generated by the BNF Converter ***/",
  "/*                                                                          */",
  "/* This test will parse a file, print the abstract syntax tree, and then    */",
  "/* pretty-print the result.                                                 */",
  "/*                                                                          */",
  "/****************************************************************************/",
  "using System;",
  "using System.IO;",
  "using " ++ namespace ++ ".Absyn;",
  "",
  "namespace " ++ namespace,
  "{",
  "  public class Test",
  "  {",
  "    public static void Main(string[] args)",
  "    {",
  "      if (args.Length > 0)",
  "      {",
  "        Stream stream = File.OpenRead(args[0]);",
  "        /* The default entry point is used. For other options see class Parser */",
  "        Parser parser = new Parser();",
  "        Scanner scanner = Scanner.CreateScanner(stream);",
  "        // Uncomment to enable trace information:",
  "        // parser.Trace shows what the parser is doing",
  "        // parser.Trace = true;",
  "        // scanner.Trace prints the tokens as they are parsed, one token per line",
  "        // scanner.Trace = true;",
  "        parser.scanner = scanner;",
  "        try",
  "        {",
  "          " ++ def ++ " parse_tree = parser.Parse" ++ def ++ "();",
  "          if (parse_tree != null)",
  "          {",
  "            Console.Out.WriteLine(\"Parse Successful!\");",
  "            Console.Out.WriteLine(\"\");",
  "            Console.Out.WriteLine(\"[Abstract Syntax]\");",
  "            Console.Out.WriteLine(\"{0}\", PrettyPrinter.Show(parse_tree));",
  "            Console.Out.WriteLine(\"\");",
  "            Console.Out.WriteLine(\"[Linearized Tree]\");",
  "            Console.Out.WriteLine(\"{0}\", PrettyPrinter.Print(parse_tree));",
  "          }",
  "          else",
  "          {",
  "            Console.Out.WriteLine(\"Parse NOT Successful!\");",
  "          }",
  "        }",
  "        catch(Exception e)",
  "        {",
  "          Console.Out.WriteLine(\"Parse NOT Successful:\");",
  "          Console.Out.WriteLine(e.Message);",
  "          Console.Out.WriteLine(\"\");",
  "          Console.Out.WriteLine(\"Stack Trace:\");",
  "          Console.Out.WriteLine(e.StackTrace);",
  "        }",
  "      }",
  "      else",
  "      {",
  "        Console.Out.WriteLine(\"You must specify a filename!\");",
  "      }",
  "    }",
  "  }",
  "}"
  ]
  where
   def = head (allEntryPoints cf)

filepathtonamespace :: FilePath -> Namespace
filepathtonamespace file = take (length file - 3) (takeFileName file)

projectguid :: IO String
projectguid = do
  maybeFilePath <- findDirectory
  guid <- maybe getBadGUID getGoodGUID maybeFilePath 
  return guid
  where
    getBadGUID :: IO String
    getBadGUID = do
      putStrLn "-----------------------------------------------------------------------------"
      putStrLn "Could not find Visual Studio tool uuidgen.exe to generate project GUID!"
      putStrLn "You might want to put this tool in your PATH."
      putStrLn "-----------------------------------------------------------------------------"
      return "{00000000-0000-0000-0000-000000000000}"
    getGoodGUID :: FilePath -> IO String
    getGoodGUID filepath = do 
      let filepath' = "\"" ++ filepath ++ "\""
      (hIn, hOut, hErr, processHandle) <- runInteractiveCommand filepath'
      guid <- hGetLine hOut
      return ('{' : init guid ++ "}")
    findDirectory :: IO (Maybe FilePath)
    findDirectory = do
      -- This works with Visual Studio 2005. 
      -- We will probably have to be modify this to include another environment variable name for Orcas. 
      -- I doubt there is any need to support VS2003? (I doubt they have patched it up to have 2.0 support?)
      toolpath <- catchIOError (getEnv "VS80COMNTOOLS") (\_ -> return "C:\\Program Files\\Microsoft Visual Studio 8\\Common7\\Tools")
      exists <- doesDirectoryExist toolpath
      if exists 
        then return (Just (toolpath ++ "\\uuidgen.exe"))
        -- this handles the case when the user was clever enough to add the directory to his/her PATH
        else findExecutable "uuidgen.exe"
