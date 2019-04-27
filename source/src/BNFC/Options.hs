{-# LANGUAGE LambdaCase #-}

module BNFC.Options where

import Data.Maybe      (fromMaybe, maybeToList)
import Data.Version    (showVersion )

import System.Console.GetOpt
import System.FilePath (takeBaseName)

import Text.Printf     (printf)

import Paths_BNFC      (version)
import BNFC.CF         (CF)
import BNFC.Utils      (unless)

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
    | Target SharedOptions FilePath
  deriving (Eq,Show,Ord)

-- | Target languages
data Target = TargetC | TargetCpp | TargetCppNoStl | TargetCSharp
            | TargetHaskell | TargetHaskellGadt | TargetLatex
            | TargetJava | TargetOCaml | TargetProfile | TargetPygments
  deriving (Eq, Bounded, Enum, Ord)

-- | List of all targets (using the enum and bounded classes).
targets :: [Target]
targets = [minBound..]

-- | List of Haskell target.
haskellTargets :: [Target]
haskellTargets = [ TargetHaskell, TargetHaskellGadt, TargetProfile ]

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
  show TargetPygments     = "Pygments"

-- | Which version of Alex is targeted?
data AlexVersion = Alex1 | Alex2 | Alex3
  deriving (Show,Eq,Ord,Bounded,Enum)

-- | Happy modes
data HappyMode = Standard | GLR
  deriving (Eq,Show,Bounded,Enum,Ord)

data JavaLexerParser = JLexCup | JFlexCup | Antlr4
    deriving (Eq,Show,Ord)

data RecordPositions = RecordPositions | NoRecordPositions
    deriving (Eq,Show,Ord)

-- | This is the option record that is passed to the different backends.
data SharedOptions = Options
  --- Option shared by at least 2 backends
  { lbnfFile    :: FilePath        -- ^ The input file BNFC processes.
  , lang        :: String          -- ^ The language we generate: the basename of 'lbnfFile'.
  , outDir      :: FilePath        -- ^ Target directory for generated files.
  , target      :: Target          -- ^ E.g. @--haskell@.
  , make        :: Maybe String    -- ^ The name of the Makefile to generate or Nothing for no Makefile.
  , inPackage   :: Maybe String    -- ^ The hierarchical package to put the modules in, or Nothing.
  , linenumbers :: RecordPositions -- ^ Add and set line_number field for syntax classes
  --- Haskell specific:
  , inDir         :: Bool        -- ^ Option @-d@.
  , ghcExtensions :: Bool        -- ^ Option @--ghc@.
  , functor       :: Bool        -- ^ Option @--functor@.  Make AST functorial?
  , alexMode      :: AlexVersion -- ^ Options @--alex@.
  , shareStrings  :: Bool        -- ^ Option @--sharestrings@.
  , byteStrings   :: Bool        -- ^ Option @--bytestrings@.
  , glr           :: HappyMode   -- ^ Happy option @--glr@.
  , xml           :: Int         -- ^ Options @--xml@, generate DTD and XML printers.
  , cnf           :: Bool        -- ^ Option @--cnf@. Generate CNF-like tables?
  , agda          :: Bool        -- ^ Option @--agda@. Create bindings for Agda?
  --- Java specific
  , javaLexerParser :: JavaLexerParser
  --- C# specific
  , visualStudio  :: Bool        -- ^ Generate Visual Studio solution/project files.
  , wcf           :: Bool        -- ^ Windows Communication Foundation.
  } deriving (Eq, Ord, Show)

-- | We take this oportunity to define the type of the backend functions.
type Backend = SharedOptions  -- ^ Options
            -> CF             -- ^ Grammar
            -> IO ()

defaultOptions :: SharedOptions
defaultOptions = Options
  { lbnfFile        = error "lbnfFile not set"
  , lang            = error "lang not set"
  , outDir          = "."
  , target          = TargetHaskell
  , make            = Nothing
  , inPackage       = Nothing
  , linenumbers     = NoRecordPositions
  -- Haskell specific
  , inDir           = False
  , ghcExtensions   = False
  , functor         = False
  , alexMode        = Alex3
  , shareStrings    = False
  , byteStrings     = False
  , glr             = Standard
  , xml             = 0
  , cnf             = False
  , agda            = False
  -- Java specific
  , javaLexerParser = JLexCup
  -- C# specific
  , visualStudio    = False
  , wcf             = False
  }

-- | Check whether an option is unchanged from the default.
isDefault :: (Eq a)
  => (SharedOptions -> a)  -- ^ Option field name.
  -> SharedOptions         -- ^ Options.
  -> Bool
isDefault flag opts = flag opts == flag defaultOptions

-- | Return something in case option differs from default.
unlessDefault :: (Monoid m, Eq a)
  => (SharedOptions -> a)  -- ^ Option field name.
  -> SharedOptions         -- ^ Options.
  -> (a -> m)              -- ^ Action in case option differs from standard.
  -> m
unlessDefault flag opts f = unless (o == flag defaultOptions) $ f o
  where o = flag opts

-- -- | Return something in case option is unchanged from default.
-- whenDefault :: (Monoid m, Eq a)
--   => (SharedOptions -> a)  -- ^ Option field name.
--   -> SharedOptions         -- ^ Options.
--   -> m                     -- ^ Action in case option is unchanged from standard.
--   -> m
-- whenDefault flag opts m = when (o == flag defaultOptions) m
--   where o = flag opts

-- | Print options as input to BNFC.
--
-- @unwords [ "bnfc", printOptions opts ]@ should call bnfc with the same options
-- as the current instance.
--
printOptions :: SharedOptions -> String
printOptions opts = unwords . concat $
  [ [ printTargetOption tgt ]
  -- General and shared options:
  , unlessDefault outDir opts $ \ o -> [ "--outputdir=" ++ o ]
  , [ "--makefile=" ++ m  | m <- maybeToList $ make opts        ]
  , [ "-p " ++ p          | p <- maybeToList $ inPackage opts   ]
  , unlessDefault linenumbers opts $ const [ "-l" ]
  -- Haskell options:
  , [ "-d"                | inDir opts                          ]
  , [ "--ghc"             | ghcExtensions opts                  ]
  , [ "--functor"         | functor opts                        ]
  , unlessDefault alexMode opts $ \ o -> [ printAlexOption o ]
  , [ "--sharestrings"    | shareStrings opts                   ]
  , [ "--bytestrings"     | byteStrings opts                    ]
  , [ "--glr"             | glr opts == GLR                     ]
  , [ "--xml"             | xml opts == 1                       ]
  , [ "--xmlt"            | xml opts == 2                       ]
  , [ "--cnf"             | cnf opts                            ]
  , [ "--agda"            | agda opts                           ]
  -- C# options:
  , [ "--vs"              | visualStudio opts                   ]
  , [ "--wfc"             | wcf opts                            ]
  -- Java options:
  , unlessDefault javaLexerParser opts $ \ o -> [ printJavaLexerParserOption o ]
  -- Grammar file:
  , [ lbnfFile opts ]
  ]
  where
  tgt = target opts
  haskell = tgt `elem` haskellTargets

-- | Print target as an option to BNFC.
printTargetOption :: Target -> String
printTargetOption = ("--" ++) . \case
  TargetC           -> "c"
  TargetCpp         -> "cpp"
  TargetCppNoStl    -> "cpp-nostl"
  TargetCSharp      -> "csharp"
  TargetHaskell     -> "haskell"
  TargetHaskellGadt -> "haskell-gadt"
  TargetLatex       -> "latex"
  TargetJava        -> "java"
  TargetOCaml       -> "ocaml"
  TargetProfile     -> "profile"
  TargetPygments    -> "pygments"

printAlexOption :: AlexVersion -> String
printAlexOption = ("--" ++) . \case
  Alex1 -> "alex1"
  Alex2 -> "alex2"
  Alex3 -> "alex3"

printJavaLexerParserOption :: JavaLexerParser -> String
printJavaLexerParserOption = ("--" ++) . \case
  JLexCup  -> "jlex"
  JFlexCup -> "jflex"
  Antlr4   -> "antlr4"

-- ~~~ Option definition ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- This defines bnfc's "global" options, like --help
globalOptions :: [ OptDescr Mode ]
globalOptions = [
  Option [] ["help"]                      (NoArg Help)         "show help",
  Option [] ["version","numeric-version"] (NoArg Version)      "show version number"]

-- | Options for the target languages
-- targetOptions :: [ OptDescr Target ]
targetOptions :: [ OptDescr (SharedOptions -> SharedOptions)]
targetOptions =
  [ Option "" ["java"]          (NoArg (\o -> o {target = TargetJava}))
    "Output Java code [default: for use with JLex and CUP]"
  , Option "" ["haskell"]       (NoArg (\o -> o {target = TargetHaskell}))
    "Output Haskell code for use with Alex and Happy (default)"
  , Option "" ["haskell-gadt"]  (NoArg (\o -> o {target = TargetHaskellGadt}))
    "Output Haskell code which uses GADTs"
  , Option "" ["latex"]         (NoArg (\o -> o {target = TargetLatex}))
    "Output LaTeX code to generate a PDF description of the language"
  , Option "" ["c"]             (NoArg (\o -> o {target = TargetC}))
    "Output C code for use with FLex and Bison"
  , Option "" ["cpp"]           (NoArg (\o -> o {target = TargetCpp}))
    "Output C++ code for use with FLex and Bison"
  , Option "" ["cpp-nostl"]     (NoArg (\o -> o {target = TargetCppNoStl}))
    "Output C++ code (without STL) for use with FLex and Bison"
  , Option "" ["csharp"]        (NoArg (\o -> o {target = TargetCSharp}))
    "Output C# code for use with GPLEX and GPPG"
  , Option "" ["ocaml"]         (NoArg (\o -> o {target = TargetOCaml}))
    "Output OCaml code for use with ocamllex and ocamlyacc"
  , Option "" ["profile"]       (NoArg (\o -> o {target = TargetProfile}))
    "Output Haskell code for rules with permutation profiles"
  , Option "" ["pygments"]      (NoArg (\o -> o {target = TargetPygments}))
    "Output a Python lexer for Pygments"
  ]

-- | A list of the options and for each of them, the target language
-- they apply to.
specificOptions :: [(OptDescr (SharedOptions -> SharedOptions), [Target])]
specificOptions =
  [ ( Option ['l'] [] (NoArg (\o -> o {linenumbers = RecordPositions}))
          "Add and set line_number field for all syntax classes\nJava requires cup 0.11b-2014-06-11 or greater"
    , [TargetCpp, TargetJava] )
  , ( Option ['p'] []
      (ReqArg (\n o -> o {inPackage = Just n}) "<namespace>")
          "Prepend <namespace> to the package/module name"
    , [TargetCpp, TargetCSharp, TargetJava] ++ haskellTargets)
  -- Java backend:
  , ( Option [] ["jlex"  ] (NoArg (\o -> o {javaLexerParser = JLexCup}))
          "Lex with JLex, parse with CUP (default)"
    , [TargetJava] )
  , ( Option [] ["jflex" ] (NoArg (\o -> o {javaLexerParser = JFlexCup}))
          "Lex with JFlex, parse with CUP"
    , [TargetJava] )
  , ( Option [] ["antlr4"] (NoArg (\o -> o {javaLexerParser = Antlr4}))
          "Lex and parse with antlr4"
    , [TargetJava] )
  -- C++ backend:
  , ( Option [] ["vs"] (NoArg (\o -> o {visualStudio = True}))
          "Generate Visual Studio solution/project files"
    , [TargetCSharp] )
  , ( Option [] ["wcf"] (NoArg (\o -> o {wcf = True}))
          "Add support for Windows Communication Foundation,\n by marking abstract syntax classes as DataContracts"
    , [TargetCSharp] )
  -- Haskell backends:
  , ( Option ['d'] [] (NoArg (\o -> o {inDir = True}))
          "Put Haskell code in modules Lang.* instead of Lang*"
    , haskellTargets )
  , ( Option []    ["alex1"] (NoArg (\o -> o {alexMode = Alex1}))
          "Use Alex 1.1 as Haskell lexer tool"
    , haskellTargets )
  , ( Option []    ["alex2"] (NoArg (\o -> o {alexMode = Alex2}))
          "Use Alex 2 as Haskell lexer tool"
    , haskellTargets )
  , ( Option []    ["alex3"] (NoArg (\o -> o {alexMode = Alex3}))
          "Use Alex 3 as Haskell lexer tool (default)"
    , haskellTargets )
  , ( Option []    ["sharestrings"] (NoArg (\o -> o {shareStrings = True}))
          "Use string sharing in Alex 2 lexer"
    , haskellTargets )
  , ( Option []    ["bytestrings"] (NoArg (\o -> o {byteStrings = True}))
          "Use byte string in Alex 2 lexer"
    , haskellTargets )
  , ( Option []    ["glr"] (NoArg (\o -> o {glr = GLR}))
          "Output Happy GLR parser"
    , haskellTargets )
  , ( Option []    ["cnf"] (NoArg (\o -> o {cnf = True}))
          "Use the CNF parser instead of happy"
    , haskellTargets )
  , ( Option []    ["ghc"] (NoArg (\o -> o {ghcExtensions = True}))
          "Use ghc-specific language extensions"
    , haskellTargets )
  , ( Option []    ["functor"] (NoArg (\o -> o {functor = True}))
          "Make the AST a functor and use it to store the position of the nodes"
    , [TargetHaskell] )
  , ( Option []    ["xml"] (NoArg (\o -> o {xml = 1}))
          "Also generate a DTD and an XML printer"
    , haskellTargets )
  , ( Option []    ["xmlt"] (NoArg (\o -> o {xml = 2}))
          "DTD and an XML printer, another encoding"
    , haskellTargets )
  , ( Option []    ["agda"] (NoArg (\o -> o {agda = True}))
          "Also generate Agda bindings for the abstract syntax"
    , haskellTargets )
  ]

-- | The list of specific options for a target.
specificOptions' :: Target -> [OptDescr (SharedOptions -> SharedOptions)]
specificOptions' t = map fst $ filter (elem t . snd) specificOptions

commonOptions :: [OptDescr (SharedOptions -> SharedOptions)]
commonOptions =
  [ Option "m" ["makefile"] (OptArg (setMakefile . fromMaybe "Makefile") "MAKEFILE")
      "generate Makefile"
  , Option "o" ["outputdir"] (ReqArg (\n o -> o {outDir = n}) "DIR")
      "Redirects all generated files into DIR"
  ]
  where setMakefile mf o = o { make = Just mf }

allOptions :: [OptDescr (SharedOptions -> SharedOptions)]
allOptions = targetOptions ++ commonOptions ++ map fst specificOptions

-- | All target options and all specific options for a given target.
allOptions' :: Target -> [OptDescr (SharedOptions -> SharedOptions)]
allOptions' t = targetOptions ++ commonOptions ++ specificOptions' t

-- ~~~ Help strings ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

title :: [String]
title =
  [ "The BNF Converter, " ++ showVersion version ++ " (c) 2002-today BNFC development team."
  , "Free software under GNU General Public License (GPL)."
  , "List of recent contributors at https://github.com/BNFC/bnfc/graphs/contributors."
  , "Report bugs at https://github.com/BNFC/bnfc/issues."
  , ""
  ]

oldContributors :: [String]
oldContributors =
  [ "(c) Jonas Almström Duregård, Krasimir Angelov, Jean-Philippe Bernardy, Björn Bringert, Johan Broberg, Paul Callaghan, "
  , "    Grégoire Détrez, Markus Forsberg, Ola Frid, Peter Gammie, Thomas Hallgren, Patrik Jansson, "
  , "    Kristofer Johannisson, Antti-Juhani Kaijanaho, Ulf Norell, "
  , "    Michael Pellauer and Aarne Ranta 2002 - 2013."
  ]

usage :: String
usage = "usage: bnfc [--version] [--help] <target language> [<args>] file.cf"

help :: String
help = unlines $ title ++
    [ usage
    , ""
    , usageInfo "Global options"   globalOptions
    , usageInfo "Common options"   commonOptions
    , usageInfo "Target languages" targetOptions
    ] ++ map targetUsage helpTargets
  where
  helpTargets = [TargetHaskell, TargetJava, TargetCpp, TargetCSharp ]
  targetUsage t = usageInfo
    (printf "Special options for the %s backend" (show t))
    (specificOptions' t)

-- ~~~ Parsing machinery ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | Main parsing function
parseMode :: [String] -> Mode
parseMode []   = Help
parseMode args =
  -- First, check for global options like --help or --version
  case getOpt' Permute globalOptions args' of
   (mode:_,_,_,_) -> mode

   -- Then, check for unrecognized options.
   _ -> case getOpt' Permute allOptions args' of
    (_,  _, [u],      _) -> UsageError $ unwords [ "Unrecognized option:" , u ]
    (_,  _, us@(_:_), _) -> UsageError $ unwords $ "Unrecognized options:" : us

    -- Then, determine target language.
    _ -> case getOpt' Permute targetOptions args' of
      -- ([]     ,_,_,_) -> UsageError "No target selected"  -- --haskell is default target
      (_:_:_,_,_,_) -> UsageError "At most one target is allowed"

      -- Finally, parse options with known target.
      (optionUpdates,_,_,_) -> let
          -- Compute target and valid options for this target.
          tgt  = target (options optionUpdates)
          opts = allOptions' tgt
        in
        case getOpt' Permute opts args' of
          (_,  _, _,      e:_) -> UsageError e
          (_,  _, [u],      _) -> UsageError $ unwords $ [ "Backend", show tgt, "does not support option", u ]
          (_,  _, us@(_:_), _) -> UsageError $ unwords $ [ "Backend", show tgt, "does not support options" ] ++ us
          (_, [], _,        _) -> UsageError "Missing grammar file"
          (optionsUpdates, [grammarFile], [], []) ->
            let opts = (options optionsUpdates)
                       { lbnfFile = grammarFile
                       , lang = takeBaseName grammarFile
                       }
            in  Target opts grammarFile
          (_,  _, _,        _) -> UsageError "Too many arguments"
  where
  args' = translateOldOptions args
  options optionsUpdates = foldl (.) id optionsUpdates defaultOptions


-- * Backward compatibility

-- | A translation function to maintain backward compatibility
--   with the old option syntax.

translateOldOptions :: [String] -> [String]
translateOldOptions = map $ \case
  "-agda"          ->  "--agda"
  "-java"          ->  "--java"
  "-java1.5"       ->  "--java"
  "-c"             ->  "--c"
  "-cpp"           ->  "--cpp"
  "-cpp_stl"       ->  "--cpp"
  "-cpp_no_stl"    ->  "--cpp-nostl"
  "-csharp"        ->  "--csharp"
  "-ocaml"         ->  "--ocaml"
  "-fsharp"        ->  "fsharp"
  "-haskell"       ->  "--haskell"
  "-prof"          ->  "--profile"
  "-gadt"          ->  "--haskell-gadt"
  "-alex1"         ->  "--alex1"
  "-alex2"         ->  "--alex2"
  "-alex3"         ->  "--alex3"
  "-sharestrings"  ->  "--sharestring"
  "-bytestrings"   ->  "--bytestring"
  "-glr"           ->  "--glr"
  "-xml"           ->  "--xml"
  "-xmlt"          ->  "--xmlt"
  "-vs"            ->  "--vs"
  "-wcf"           ->  "--wcf"
  other            ->  other
