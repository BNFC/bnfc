module BNFC.Options where

import BNFC.CF (CF)
import BNFC.WarningM
import Control.Monad (liftM, when,unless)
import Control.Monad.State
import Control.Monad.Trans (lift)
import Data.Char
import Data.List (elemIndex, foldl', sort, intercalate)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Version ( showVersion )
import ErrM
import Paths_BNFC ( version )
import System.Console.GetOpt
import System.FilePath (takeBaseName, takeFileName)
import System.IO (stderr, hPutStrLn,hPutStr)
import Text.Printf (printf)

-- ~~~ Option data structures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- | To decouple the option parsing from the execution of the program,
-- we introduce a data structure that holds the result of the
-- parsing of the arguments.
data Mode
    -- An error has been made by the user
    -- e.g. invalid argument/combination of arguments
    = UsageError String
    -- Basic modes: print some info and exits
    | Help | Version
    -- Normal mode, specifying the back end to use,
    -- the option record to be passed to the backend
    -- and the path of the input grammar file
    | Target Target SharedOptions FilePath
  deriving (Eq,Show,Ord)

-- | Target languages
data Target = TargetC | TargetCpp | TargetCppNoStl | TargetCSharp
            | TargetHaskell | TargetHaskellGadt | TargetLatex
            | TargetJava | TargetOCaml | TargetProfile
  deriving (Eq,Bounded, Enum,Ord)

instance Show Target where
  show TargetC            = "C"
  show TargetCpp          = "C++"
  show TargetCppNoStl     = "C++ (without STL)"
  show TargetCSharp       = "C#"
  show TargetHaskell      = "Haskell"
  show TargetHaskellGadt  = "Haskell (with GADT)"
  show TargetLatex        = "Latex"
  show TargetJava         = "Java"
  show TargetOCaml        = "OCaml"
  show TargetProfile      = "Haskell (with permutation profiles)"

-- | Which version of Alex is targeted?
data AlexVersion = Alex1 | Alex2 | Alex3
  deriving (Show,Eq,Ord,Bounded,Enum)

-- | Happy modes
data HappyMode = Standard | GLR
  deriving (Eq,Show,Bounded,Enum,Ord)

-- | This is the option record that is passed to the different backends
data SharedOptions = Options
  -- Option shared by at least 2 backends
  { make :: Bool
  , inPackage :: Maybe String -- ^ The hierarchical package to put
                              --   the modules in, or Nothing.
  , cnf :: Bool               -- ^ Generate CNF-like tables?
  , lang :: String
  -- Haskell specific:
  , alexMode :: AlexVersion
  , inDir :: Bool
  , shareStrings :: Bool
  , byteStrings :: Bool
  , glr :: HappyMode
  , xml :: Int
  , deriveDataTypeable :: Bool
  , deriveGeneric :: Bool
  -- C++ specific
  , linenumbers :: Bool       -- ^ Add and set line_number field for syntax classes
  -- C# specific
  , visualStudio :: Bool      -- ^ Generate Visual Studio solution/project files
  , wcf :: Bool               -- ^ Windows Communication Foundation
  } deriving (Eq,Show,Ord)

-- | We take this oportunity to define the type of the backend functions
type Backend = SharedOptions  -- ^ options
            -> CF             -- ^ Grammar
            -> IO ()

defaultOptions = Options
  { cnf = False
  , inPackage = Nothing
  , make = False
  , alexMode = Alex3
  , inDir = False
  , shareStrings = False
  , byteStrings = False
  , glr = Standard
  , xml = 0
  , deriveDataTypeable = False
  , deriveGeneric = False
  , lang = ""
  , linenumbers = False
  , visualStudio = False
  , wcf = False }

-- ~~~ Option definition ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- This defines bnfc's "global" options, like --help
globalOptions :: [ OptDescr Mode ]
globalOptions = [
  Option [] ["help"]          (NoArg Help)         "show help",
  Option [] ["version"]       (NoArg Version)      "show version number"]

-- | Options for the target languages
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

-- | For each target, describes the allowed options
specificOptions :: Target -> [ OptDescr (SharedOptions -> SharedOptions)]
specificOptions target = case target of
    TargetCpp ->
      [ Option ['l'] [] (NoArg (\o -> o {linenumbers = True}))
          "Add and set line_number field for all syntax classes"
      , Option ['p'] []
          (ReqArg (\n o -> o {inPackage = Just n}) "<namespace>")
          "Use <namespace> as the C++ namespace" ]
    TargetCSharp  ->
      [ Option ['p'] []
          (ReqArg (\n o -> o {inPackage = Just n}) "<namespace>")
          "Use <namespace> as the C# namespace"
      , Option [] ["vs"] (NoArg (\o -> o {visualStudio = True}))
          "Generate Visual Studio solution/project files"
      , Option [] ["wcf"] (NoArg (\o -> o {wcf = True}))
          "Add support for Windows Communication Foundation,\n by marking abstract syntax classes as DataContracts" ]
    TargetHaskell ->
      [ Option ['d'] [] (NoArg (\o -> o {inDir = True}))
          "Put Haskell code in modules Lang.* instead of Lang*"
      , Option ['p'] [] (ReqArg (\n o -> o {inPackage = Just n}) "<name>")
          "Prepend <name> to the Haskell module names."
      , Option []    ["alex1"] (NoArg (\o -> o {alexMode = Alex1}))
          "Use Alex 1.1 as Haskell lexer tool"
      , Option []    ["alex2"] (NoArg (\o -> o {alexMode = Alex2}))
          "Use Alex 2 as Haskell lexer tool"
      , Option []    ["alex3"] (NoArg (\o -> o {alexMode = Alex3}))
          "Use Alex 3 as Haskell lexer tool (default)"
      , Option []    ["sharestrings"] (NoArg (\o -> o {shareStrings = True}))
          "Use string sharing in Alex 2 lexer"
      , Option []    ["bytestrings"] (NoArg (\o -> o {byteStrings = True}))
          "Use byte string in Alex 2 lexer"
      , Option []    ["glr"] (NoArg (\o -> o {glr = GLR}))
          "Output Happy GLR parser"
      , Option []    ["xml"] (NoArg (\o -> o {xml = 1}))
          "Also generate a DTD and an XML printer"
      , Option []    ["xmlt"] (NoArg (\o -> o {xml = 2}))
          "DTD and an XML printer, another encoding"
      , Option []    ["cnf"] (NoArg (\o -> o {cnf = True}))
          "Use the CNF parser instead of happy"
      , Option []    ["deriveDataTypeable"] (NoArg (\o -> o {deriveDataTypeable = True}))
          "Derive Data and Typeable instances for AST types"
      , Option []    ["deriveGeneric"] (NoArg (\o -> o {deriveGeneric = True}))
          "Derive Generic instances for AST types" ]
    TargetJava ->
      [ Option ['p'] [] (ReqArg (\n o -> o {inPackage = Just n}) "<package>")
          "Prepend <package> to the Java package name" ]
    TargetHaskellGadt -> specificOptions TargetHaskell
    TargetProfile     -> specificOptions TargetHaskell
    _ -> []

makefileOption =
  [ Option "m" [] (NoArg True) "generate Makefile" ]

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
help = unlines $
    usage:""
    :usageInfo "Global options"   globalOptions
    :usageInfo "Make option"      makefileOption
    :usageInfo "Target languages" targetOptions
    :map targetUsage targets
  where targets = [TargetHaskell, TargetJava, TargetCpp, TargetCSharp ]
        targetUsage t = usageInfo
                        (printf "Special options for the %s backend" (show t))
                        (specificOptions t)

-- ~~~ Parsing machinery ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | Main parsing function
parseMode :: [String] -> Mode
parseMode args = runOptParse (translateOldOptions args) $ do
    -- Firts, if the list of args is empty, it is not considered an error
    -- and we simply print the help screen
    when (null args) $ setmode Help

    -- Next we check the presence of --help or --version
    -- and, if found, we return the corresponding mode and ignore the rest of
    -- the options
    opts <- parseOpt globalOptions
    -- The order in which Mode constructors are defined set their priority
    case listToMaybe (sort opts) of
      Just mode -> setmode mode
      Nothing -> return () -- continue analysing

    -- Now we look for the target language in the options:
    target <- do
      targets <- parseOpt targetOptions
      case targets of
        []  -> return TargetHaskell
        [t] -> return t
        _   -> usageError "only one target language is allowed"

    -- makefile option
    makefile <- parseOpt makefileOption >>= return . or

    -- Now we can parse the target specific options and
    -- build the SharedOption record
    options <- do
      optionsUpdates <- parseOpt (specificOptions target)
      return $ foldl (.) id optionsUpdates defaultOptions

    -- Finally, we get the cf file from the remaining arguments
    args <- get
    cfFile <- case getOpt' Permute [] args of
      (_,[f],[],_) -> return f
      (_,_,o:_,_)  -> usageError $ "Unrecognized option " ++ o
      (_,[],_,_)   -> usageError "Missing grammar file"
      (_,_,_,_)    -> usageError "Too many arguments"
    setmode $ Target target (options {lang = takeBaseName cfFile, make = makefile}) cfFile

-- Option parsing monad:
type OptParse = StateT [String] (Either Mode)

-- run a computation in the OptParse monad
runOptParse :: [String] -> OptParse () -> Mode
runOptParse args c = case runStateT c args of
  Left  mode      -> mode
  Right ((), _)  -> Help

-- Set the mode and interupt the parsing
setmode :: Mode -> OptParse a
setmode = lift . Left
-- Shortcut to set the mode to UsageError
usageError :: String -> OptParse a
usageError = setmode . UsageError

-- Lift myGetOpt' in the option parsing monad
parseOpt :: [OptDescr a] -> OptParse [a]
parseOpt optDescr = do
    args <- get
    let (as, arg', errors) = myGetOpt' optDescr args
    unless (null errors) $ usageError (head errors)
    put arg'
    return as

-- Variant of getOpt' that does not separate non-options from unrecognized options
myGetOpt' :: [OptDescr a] -> [String] -> ([a], [String], [String])
myGetOpt' descr args = case getOpt' RequireOrder descr args of
  -- getOpt' returns 4 lists: the first one is the list of parsed values
  --  the second one is the list of everything after the first non-option
  --  the 3rd is the list of unrecognized options
  --  the last is the list of error messages
  -- Here we are going to call getOpt' multiple times until the second list
  -- is empty
  (as, [], skipped, err) -> (as, skipped, err)
  (as, nonopt, skipped, err) ->
    let (as', skipped', err') = myGetOpt' descr (tail nonopt) in
    (as ++ as', skipped ++ [head nonopt] ++ skipped', err ++ err')

isUsageError :: Mode -> Bool
isUsageError (UsageError _) = True
isUsageError _ = False

-- ~~~ Backward compatibility ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A translating function to maintain backward compatiblicy
-- with the old option syntay
translateOldOptions :: [String] -> [String]
translateOldOptions opts = concatMap translateOne opts
  where translateOne "-java" = return "--java"
        translateOne "-java1.5"       = return "--java"
        translateOne "-c"             = return "--c"
        translateOne "-cpp"           = return "--cpp"
        translateOne "-cpp_stl"       = return "--cpp"
        translateOne "-cpp_no_stl"    = return "--cpp-nostl"
        translateOne "-csharp"        = return "--csharp"
        translateOne "-ocaml"         = return "--ocaml"
        translateOne "-fsharp"        = return "fsharp"
        translateOne "-haskell"       = return "--haskell"
        translateOne "-prof"          = return "--profile"
        translateOne "-gadt"          = return "--haskell-gadt"
        translateOne "-alex1"         = return "--alex1"
        translateOne "-alex2"         = return "--alex2"
        translateOne "-alex3"         = return "--alex3"
        translateOne "-sharestrings"  = return "--sharestring"
        translateOne "-bytestrings"   = return "--bytestring"
        translateOne "-glr"           = return "--glr"
        translateOne "-xml"           = return "--xml"
        translateOne "-xmlt"          = return "--xmlt"
        translateOne "-vs"            = return "--vs"
        translateOne "-wcf"           = return "--wcf"
        translateOne other            = return other

