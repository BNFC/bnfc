module BNFC.Options where

import System.Console.GetOpt

-- For the old parser
import Data.Char
import Data.Maybe (listToMaybe)
import Data.List (elemIndex, foldl', sort)
import Control.Monad (when,unless)
import System.FilePath (takeFileName)
import Control.Monad (liftM)
import BNFC.WarningM
import Text.Printf (printf)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import ErrM
import Paths_BNFC ( version )
import Data.Version ( showVersion )
import System.IO (stderr, hPutStrLn,hPutStr)
-- Allowed extensions for grammar files
allowed_exts = [ "cf", "bnf", "lbnf", "bnfc" ]

data AlexVersion = Alex1 | Alex2 | Alex3
  deriving (Show,Eq,Ord)

data Target = TargetC
            | TargetCpp
            | TargetCppNoStl
            | TargetCSharp
            | TargetHaskell
            | TargetHaskellGadt
            | TargetLatex
            | TargetJava
            | TargetOCaml
            | TargetProfile
  deriving (Eq,Show, Bounded, Enum)

-- | Which version of Alex is targeted?
data HappyMode = Standard | GLR
  deriving (Eq,Show)

data SharedOptions = Options
    {
     targets :: [Target],
     make :: Bool,
     alexMode :: AlexVersion,
     inDir :: Bool,
     shareStrings :: Bool,
     byteStrings :: Bool,
     glr :: HappyMode,
     xml :: Int,
     inPackage :: Maybe String, -- ^ The hierarchical package to put
	                        --   the modules in, or Nothing.
     lang :: String, -- ^ Prefix to use in module names
     file :: FilePath, -- ^ Path of the input file 
     multi :: Bool,
     cnf :: Bool -- ^ Generate CNF-like tables?
    }
  deriving (Eq,Show)

defaultOptions = Options 
  { make = False, alexMode = Alex3, inDir = False, shareStrings = False
  , byteStrings = False, glr = Standard, xml = 0, inPackage = Nothing
  , file="", lang = "", multi = False, cnf = False, targets = [TargetHaskell] }

anyTarget opts vs = any (isOpt opts) vs
  where isOpt     opts v  = elem v $ targets opts

name :: SharedOptions -> String
name = lang

parseArguments :: Monad m => [String] -> m SharedOptions
parseArguments args' = do
  let args = (map (filter (not . isSpace)) args')
  let file = last args
  if (head file == '-') then fail "Missing grammar file"
   else do
      let name = takeWhile (/= '.') $ takeFileName file
      let make = elem "-m" args
      let multi = elem "--multilingual" args
      let c = elem "-c" args
      let cpp_no_stl = elem "-cpp_no_stl" args
      let cpp_stl = elem "-cpp_stl" args || elem "-cpp" args
      let csharp = elem "-csharp" args
      let java = elem "-java1.5" args || elem "-java" args
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
			      fail "-p option requires an argument"
      let options = Options {make = make,
                             alexMode = alexMode,
                             inDir = inDir,
                             shareStrings = alex2StringSharing,
                             byteStrings = alex2ByteString,
                             glr = if glr then GLR else Standard,
                             xml = xml,
                             inPackage = inPackage,
                             lang = name,
                             file = file,
                             multi = multi,
                             cnf = elem "-cnf" args,
                             targets = targets
                             }
          targets0 = [ TargetC            | c           ]
                  ++ [ TargetCppNoStl     | cpp_no_stl  ]
                  ++ [ TargetCpp          | cpp_stl     ]
                  ++ [ TargetCSharp       | csharp      ]
                  ++ [ TargetHaskellGadt  | haskellGADT ]
                  ++ [ TargetJava         | java        ]
                  ++ [ TargetOCaml        | ocaml       ]
                  ++ [ TargetProfile      | profile     ]
          targets = if null targets0 then [TargetHaskell] else targets0
      unless (length targets == 1) $
        fail "Error: only one language mode may be chosen"
      unless (isCfFile file) $
        fail "Error: the input file must end with .cf"
      return options

isCfFile = isCF . reverse
  where isCF ('f':'c':'.':_)     = True
        isCF ('f':'n':'b':'.':_) = True
        isCF ('f':'n':'b':'l':'.':_) = True
        isCF ('c':'f':'n':'b':'.':_) = True
        isCF _                   = False


-- Backward compatibility: This function makes sure that we stay backward
-- compatible wrt. the old argument names by translating the old arguments
-- to their new equivalent. In addition, it produces wornings messages
-- to be displayed to the user
translateArguments :: [String] -> WithWarnings [String]
translateArguments
  = concatMapM $ translate
      [ ( "-java1.4", ["--java4"])
      , ( "-java1.5", ["--java5"])
      , ( "-java", ["--java"])
      , ( "-c", ["--c"])
      , ( "-cpp", ["--cpp"])
      , ( "-cpp_stl", ["--cpp","--stl"])
      , ( "-cpp_no_stl", ["--cpp","--no-stl"])
      , ( "-csharp", ["--csharp"])
      , ( "-ocaml", ["--ocaml"])
      , ( "-haskell", ["--haskell"])
      , ( "-prof", ["--haskell","--prof"])
      , ( "-gadt", ["--haskell","--gadt"])
      , ( "-alex1", ["--alex1"])
      , ( "-alex2", ["--alex2"])
      , ( "-alex3", ["--alex3"])
      , ( "-sharestrings", ["--sharestrings"])
      , ( "-bytestrings", ["--bytestrings"])
      , ( "-glr", ["--glr"])
      , ( "-bytestrings", ["--bytestrings"])
      , ( "-xml", ["--xml"])
      , ( "-xmlt", ["--xmlt"])
      , ( "-vs", ["--vs"])
      , ( "-wcf", ["--wcf"])
      ]
  where translate :: [(String,[String])] -> String -> WithWarnings [String]
        translate map s
          = case lookup s map of
              Nothing -> return [s]
              Just ss ->
                let msg = printf "Option %s is deprecated, use %s instead"
                                  s (intercalate " " ss)
                in warn msg >> return ss


concatMapM :: Monad m => ( a -> m [b] ) -> [a] -> m [b]
concatMapM f l = mapM f l >>= return . concat

-- | Given a list of arguments, returns a list of error messages
-- one for each deprecated argument in the original list
-- Note that the returned error messages end with a newline
-- to make them consistent with the errors reported by the GetOpt module
lookForDeprecatedOptions :: [String] -> [String]
lookForDeprecatedOptions = catMaybes . map msg
  where deprecated =  [ ("--numeric-version","--version")
                      , ("-multi", "--multilingual") ]
        msg :: String -> Maybe String
        msg arg = do
          newArg <- lookup arg deprecated
          return $ printf "%s is deprecated, use %s instead\n" arg newArg

-- | To decouple the option parsing from the execution of the program,
-- we introduce here a new data structure that holds the result of the
-- parsing of the arguments.
data Mode
  -- An error has been made by the user
  -- e.g. invalid argument/combination of arguments
  = UsageError String
  -- Basic modes: print some info and exits
  | Help | Version
  -- Normal mode, specifying the back end to use,
  -- a list of un-parsed arguments to be passed to the backend
  -- and the path of the input grammar file
  | Target Target [String] FilePath
  -- multi-mode: same as above except that more than one backend may be
  -- specified
  -- | Multi [(Target,[String])] FilePath
  deriving (Eq)
instance Show Mode where
  show Help = "--help"
  show Version = "--version"
  show (Target t args f) = unwords $ showTarget t:args ++ [f]
  show (UsageError msg) = "Error " ++ show msg

isUsageError :: Mode -> Bool
isUsageError (UsageError _) = True
isUsageError _ = False

showTarget :: Target -> String
showTarget TargetC = "--c"
showTarget TargetCppNoStl = "--cpp-nostl"
showTarget TargetCpp = "--cpp"
showTarget TargetCSharp = "--csharp"
showTarget TargetHaskell = "--haskell"
showTarget TargetHaskellGadt = "--haskell-gadt"
showTarget TargetLatex = "--latex"
showTarget TargetJava ="--java"
showTarget TargetOCaml ="--ocaml"
showTarget TargetProfile ="--profile"


-- ~~~ Option parsing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- This defines bnfc's "global" options, i.e. the options that are allowed
-- before the sub-command
data BnfcOption = OptHelp | OptVersion | OptMultilingual
  deriving (Show, Eq, Ord)
globalOptions :: [ OptDescr BnfcOption ]
globalOptions = [
  Option [] ["help"]          (NoArg OptHelp)         "show help",
  Option [] ["version"]       (NoArg OptVersion)      "show version number",
  Option [] ["multilingual"]  (NoArg OptMultilingual) "multilingual BNF" ]

targetOptions :: [ OptDescr Target ]
targetOptions =
  [ Option "" ["java"]          (NoArg TargetJava)
    "Output Java code for use with JLex and CUP"
  , Option "" ["haskell"]       (NoArg TargetHaskell)
    "Output Haskell code for use with Alex and Happy (default)"
  , Option "" ["haskell-gadt"]  (NoArg TargetHaskellGadt)
    "Output Haskell code which uses GADTs"
  , Option "" ["latex"]         (NoArg TargetLatex)
    "Output LaTeX code to generate a PDF description of the language"
  , Option "" ["c"]             (NoArg TargetC)
    "Output C code for use with FLex and Bison"
  , Option "" ["cpp"]           (NoArg TargetCpp)
    "Output C++ code for use with FLex and Bison"
  , Option "" ["cpp-nostl"]     (NoArg TargetCppNoStl)
    "Output C++ code (without STL) for use with FLex and Bison"
  , Option "" ["csharp"]        (NoArg TargetCSharp)
    "Output C# code for use with GPLEX and GPPG"
  , Option "" ["ocaml"]         (NoArg TargetOCaml)
    "Output OCaml code for use with ocamllex and ocamlyacc"
  , Option "" ["profile"]       (NoArg TargetProfile)
    "Output Haskell code for rules with permutation profiles" ]

parseMode :: [String] -> Mode
parseMode args = either id (const Help) $ do
  -- we use getOpt' which will ignore any argument it doesn't recognize
  -- This allows us to split the parsing of options in several 'layers'
  -- (first global options, then the target language. then each backend is
  -- responsible for parsing its own options)
  -- The returned tuple consist of:
  -- - option arguments
  -- - a list of non-options
  -- - a list of unrecognized options
  -- - and a list of error messages
  let (opts, files, args', errors) =  getOpt' Permute globalOptions args
  -- If getOpt' produces errors, we return the first one and stop
  unless (null errors) $ usageError (head errors)
  -- The order in which BnfcOption are defined set their
  -- priority
  let global = listToMaybe (sort opts)
  case global of
    Just OptHelp -> Left Help
    Just OptVersion -> Left Version
    Just OptMultilingual -> undefined
    Nothing -> return () -- continue analysing
  -- No arguments left to analyse --> print help
  when (null args') $ Left Help
  -- time to parse the arguments again to find the target language.
  -- Note that we know that we will get an empty list of non-options because
  -- they should all have been extracted by the last getOpt' call.
  let (targets, [], args'', errors') =  getOpt' Permute targetOptions args'
  unless (null errors')   $ usageError (head errors')
  case (targets,files) of
    -- Base case: exactly one target language and one file
    ([target],[file]) -> Left $ Target target args'' file
    -- no target
    ([], _)           -> usageError "Missing target language"
    -- too many targets
    (_,[_])           -> usageError "only one target language is allowed"
    -- no file
    (_, [])           -> usageError "Missing grammar file"
    -- too many files
    (_,_)             -> usageError "only one grammar file is allowed"
  where usageError = Left . UsageError

-- ~~~ Help strings ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
title = unlines [
  "The BNF Converter, "++showVersion version,
  "(c) Jonas Almström Duregård, Krasimir Angelov, Jean-Philippe Bernardy, Björn Bringert, Johan Broberg, Paul Callaghan, ",
  "    Grégoire Détrez, Markus Forsberg, Ola Frid, Peter Gammie, Thomas Hallgren, Patrik Jansson, ",
  "    Kristofer Johannisson, Antti-Juhani Kaijanaho, Ulf Norell, ",
  "    Michael Pellauer and Aarne Ranta 2002 - 2013.",
  "Free software under GNU General Public License (GPL).",
  "Bug reports to bnfc-dev@googlegroups.com."
 ]

usage :: String
usage = "usage: bnfc [--version] [--help] <target language> [<args>] file.cf"

help :: String
help = unlines  [ usage , "" , usageInfo "Global options" globalOptions
                , usageInfo "Target languages" targetOptions ]
