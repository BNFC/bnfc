{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances#-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module BNFC.Options
  ( Mode(..), Target(..), Backend
  , parseMode, usage, help, versionString
  , SharedOptions(..)
  , defaultOptions, isDefault, printOptions
  , AlexVersion(..), HappyMode(..), OCamlParser(..), JavaLexerParser(..)
  , RecordPositions(..), TokenText(..)
  , Ansi(..)
  , InPackage
  , removedIn290
  , translateOldOptions
  )
  where

import qualified Control.Monad as Ctrl
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Control.Monad.Except (MonadError(..))

import Data.Bifunctor
import Data.Either     (partitionEithers)
import qualified Data.Map  as Map
-- import qualified Data.List as List
import Data.Maybe      (fromMaybe, maybeToList)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup  (Semigroup(..))  -- for ghc 7.10 - 8.2
#endif
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
    | Help | License | Version
    -- Normal mode, specifying the back end to use,
    -- the option record to be passed to the backend
    -- and the path of the input grammar file
    | Target SharedOptions FilePath
  deriving (Eq,Show,Ord)

-- | Target languages
data Target = TargetC | TargetCpp | TargetCppNoStl
            | TargetHaskell | TargetHaskellGadt | TargetLatex
            | TargetJava | TargetOCaml | TargetPygments
            | TargetCheck
  deriving (Eq, Bounded, Enum, Ord)

-- | List of Haskell target.
haskellTargets :: [Target]
haskellTargets = [ TargetHaskell, TargetHaskellGadt ]

instance Show Target where
  show TargetC            = "C"
  show TargetCpp          = "C++"
  show TargetCppNoStl     = "C++ (without STL)"
  show TargetHaskell      = "Haskell"
  show TargetHaskellGadt  = "Haskell (with GADT)"
  show TargetLatex        = "Latex"
  show TargetJava         = "Java"
  show TargetOCaml        = "OCaml"
  show TargetPygments     = "Pygments"
  show TargetCheck        = "Check LBNF file"

-- | Which version of Alex is targeted?
data AlexVersion = Alex3
  deriving (Show,Eq,Ord,Bounded,Enum)

-- | Happy modes
data HappyMode = Standard | GLR
  deriving (Eq,Show,Bounded,Enum,Ord)

-- | Which parser generator for ocaml?
data OCamlParser = OCamlYacc | Menhir
    deriving (Eq,Show,Ord)

-- | Which Java backend?
data JavaLexerParser = JLexCup | JFlexCup | Antlr4
    deriving (Eq,Show,Ord)

-- | Line numbers or not?
data RecordPositions = RecordPositions | NoRecordPositions
    deriving (Eq,Show,Ord)

-- | Restrict to ANSI standard (C/C++)?
data Ansi = Ansi | BeyondAnsi
    deriving (Eq, Ord, Show)

-- | Package name (C++ and Java backends).
type InPackage = Maybe String

-- | How to represent token content in the Haskell backend?

data TokenText
  = StringToken      -- ^ Represent strings as @String@.
  | ByteStringToken  -- ^ Represent strings as @ByteString@.
  | TextToken        -- ^ Represent strings as @Data.Text@.
  deriving (Eq, Ord, Show)

-- | This is the option record that is passed to the different backends.
data SharedOptions = Options
  --- Option shared by at least 2 backends
  { lbnfFile    :: FilePath        -- ^ The input file BNFC processes.
  , lang        :: String          -- ^ The language we generate: the basename of 'lbnfFile'.
  , outDir      :: FilePath        -- ^ Target directory for generated files.
  , force       :: Bool            -- ^ Ignore errors as much as possible?
  , target      :: Target          -- ^ E.g. @--haskell@.
  , make        :: Maybe String    -- ^ The name of the Makefile to generate or Nothing for no Makefile.
  , inPackage   :: InPackage       -- ^ The hierarchical package to put the modules in, or Nothing.
  , linenumbers :: RecordPositions -- ^ Add and set line_number field for syntax classes
  , ansi        :: Ansi            -- ^ Restrict to the ANSI language standard (C/C++)?
  --- Haskell specific:
  , inDir         :: Bool        -- ^ Option @-d@.
  , functor       :: Bool        -- ^ Option @--functor@.  Make AST functorial?
  , generic       :: Bool        -- ^ Option @--generic@.  Derive Data, Generic, Typeable?
  , alexMode      :: AlexVersion -- ^ Options @--alex@.
  , tokenText     :: TokenText   -- ^ Options @--bytestrings@, @--string-token@, and @--text-token@.
  , glr           :: HappyMode   -- ^ Happy option @--glr@.
  , xml           :: Int         -- ^ Options @--xml@, generate DTD and XML printers.
  , agda          :: Bool        -- ^ Option @--agda@. Create bindings for Agda?
  --- OCaml specific
  , ocamlParser   :: OCamlParser -- ^ Option @--menhir@ to switch to @Menhir@.
  --- Java specific
  , javaLexerParser :: JavaLexerParser
  --- C# specific
  , visualStudio  :: Bool        -- ^ Generate Visual Studio solution/project files.
  , wcf           :: Bool        -- ^ Windows Communication Foundation.
  } deriving (Eq, Ord, Show)

-- We take this opportunity to define the type of the backend functions.
type Backend = SharedOptions  -- ^ Options
            -> CF             -- ^ Grammar
            -> IO ()

defaultOptions :: SharedOptions
defaultOptions = Options
  { lbnfFile        = error "lbnfFile not set"
  , lang            = error "lang not set"
  , outDir          = "."
  , force           = False
  , target          = TargetHaskell
  , make            = Nothing
  , inPackage       = Nothing
  , linenumbers     = NoRecordPositions
  , ansi            = BeyondAnsi
  -- Haskell specific
  , inDir           = False
  , functor         = False
  , generic         = False
  , alexMode        = Alex3
  , tokenText       = StringToken
  , glr             = Standard
  , xml             = 0
  , agda            = False
  -- OCaml specific
  , ocamlParser     = OCamlYacc
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
  , unlessDefault ansi opts $ const [ "--ansi" ]
  -- Haskell options:
  , [ "-d"                | inDir opts                          ]
  , [ "--functor"         | functor opts                        ]
  , [ "--generic"         | generic opts                        ]
  , unlessDefault alexMode opts $ \ o -> [ printAlexOption o ]
  , [ "--bytestrings"     | tokenText opts == ByteStringToken   ]
  , [ "--text-token"      | tokenText opts == TextToken, not (agda opts) ]  -- default for --agda
  , [ "--string-token"    | tokenText opts == StringToken, agda opts ]      -- default unless --agda
  , [ "--glr"             | glr opts == GLR                     ]
  , [ "--xml"             | xml opts == 1                       ]
  , [ "--xmlt"            | xml opts == 2                       ]
  , [ "--agda"            | agda opts                           ]
  -- C# options:
  , [ "--vs"              | visualStudio opts                   ]
  , [ "--wfc"             | wcf opts                            ]
  -- Java options:
  , unlessDefault javaLexerParser opts $ \ o -> [ printJavaLexerParserOption o ]
  -- Java options:
  , unlessDefault ocamlParser opts $ \ o -> [ printOCamlParserOption o ]
  -- Grammar file:
  , [ lbnfFile opts ]
  ]
  where
  tgt = target opts
  -- haskell = tgt `elem` haskellTargets

-- | Print target as an option to BNFC.
printTargetOption :: Target -> String
printTargetOption = ("--" ++) . \case
  TargetC           -> "c"
  TargetCpp         -> "cpp"
  TargetCppNoStl    -> "cpp-nostl"
  TargetHaskell     -> "haskell"
  TargetHaskellGadt -> "haskell-gadt"
  TargetLatex       -> "latex"
  TargetJava        -> "java"
  TargetOCaml       -> "ocaml"
  TargetPygments    -> "pygments"
  TargetCheck       -> "check"

printAlexOption :: AlexVersion -> String
printAlexOption = ("--" ++) . \case
  Alex3 -> "alex3"

printJavaLexerParserOption :: JavaLexerParser -> String
printJavaLexerParserOption = ("--" ++) . \case
  JLexCup  -> "jlex"
  JFlexCup -> "jflex"
  Antlr4   -> "antlr4"

printOCamlParserOption :: OCamlParser -> String
printOCamlParserOption = ("--" ++) . \case
  OCamlYacc -> "yacc"
  Menhir    -> "menhir"

-- ~~~ Option definition ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- This defines bnfc's "global" options, like --help
globalOptions :: [ OptDescr Mode ]
globalOptions = [
  Option [] ["help"]                      (NoArg Help)         "show help",
  Option [] ["license"]                   (NoArg License)      "show license",
  Option [] ["version","numeric-version"] (NoArg Version)      "show version number"]

-- | Options for the target languages
-- targetOptions :: [ OptDescr Target ]
targetOptions :: [ OptDescr (SharedOptions -> SharedOptions)]
targetOptions =
  [ Option "" ["java"]          (NoArg (\o -> o {target = TargetJava}))
    "Output Java code [default: for use with JLex and CUP]"
  , Option "" ["java-antlr"]    (NoArg (\ o -> o{ target = TargetJava, javaLexerParser = Antlr4 }))
    "Output Java code for use with ANTLR (short for --java --antlr)"
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
  , Option "" ["ocaml"]         (NoArg (\o -> o {target = TargetOCaml}))
    "Output OCaml code for use with ocamllex and ocamlyacc"
  , Option "" ["ocaml-menhir"]  (NoArg (\ o -> o{ target = TargetOCaml, ocamlParser = Menhir }))
    "Output OCaml code for use with ocamllex and menhir (short for --ocaml --menhir)"
  , Option "" ["pygments"]      (NoArg (\o -> o {target = TargetPygments}))
    "Output a Python lexer for Pygments"
  , Option "" ["check"]         (NoArg (\ o -> o{target = TargetCheck }))
    "No output. Just check input LBNF file"
  ]

-- | A list of the options and for each of them, the target language
-- they apply to.
specificOptions :: [(OptDescr (SharedOptions -> SharedOptions), [Target])]
specificOptions =
  [ ( Option ['l'] ["line-numbers"] (NoArg (\o -> o {linenumbers = RecordPositions})) $ unlines
        [ "Add and set line_number field for all syntax classes"
        , "(Note: Java requires cup version 0.11b-2014-06-11 or greater.)"
        ]
    , [TargetC, TargetCpp, TargetJava] )
  , ( Option [] ["ansi"] (NoArg (\o -> o{ ansi = Ansi })) $ unlines
        [ "Restrict to ANSI language standard"
        ]
    , [TargetCpp] )  -- In the future maybe also: TargetC
  , ( Option ['p'] ["name-space"]
      (ReqArg (\n o -> o {inPackage = Just n}) "NAMESPACE")
          "Prepend NAMESPACE to the package/module name"
    , [TargetCpp, TargetJava] ++ haskellTargets)
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
  -- OCaml backend:
  , ( Option [] ["yacc"  ] (NoArg (\ o -> o { ocamlParser = OCamlYacc }))
          "Generate parser with ocamlyacc (default)"
    , [TargetOCaml] )
  , ( Option [] ["menhir"] (NoArg (\ o -> o { ocamlParser = Menhir }))
          "Generate parser with menhir"
    , [TargetOCaml] )
  -- Haskell backends:
  , ( Option ['d'] [] (NoArg (\o -> o {inDir = True}))
          "Put Haskell code in modules LANG.* instead of LANG* (recommended)"
    , haskellTargets )
  -- -- Option --alex3 is obsolete since Alex 3 is the only choice now.
  -- -- Keep this in case there will be a new lexer backend for Haskell.
  -- , ( Option []    ["alex3"] (NoArg (\o -> o {alexMode = Alex3}))
  --         "Use Alex 3 as Haskell lexer tool (default)"
  --   , haskellTargets )
  , ( Option []    ["bytestrings"] (NoArg (\o -> o { tokenText = ByteStringToken }))
          "Use ByteString in Alex lexer [deprecated, use --text-token]"
    , haskellTargets )
  , ( Option []    ["text-token"] (NoArg (\o -> o { tokenText = TextToken }))
          "Use Text in Alex lexer"
          -- "Use Text in Alex lexer (default for --agda)"
    , haskellTargets )
  , ( Option []    ["string-token"] (NoArg (\o -> o { tokenText = StringToken }))
          "Use String in Alex lexer (default)"
    , haskellTargets )
  , ( Option []    ["glr"] (NoArg (\o -> o {glr = GLR}))
          "Output Happy GLR parser [deprecated]"
    , haskellTargets )
  , ( Option []    ["functor"] (NoArg (\o -> o {functor = True}))
          "Make the AST a functor and use it to store the position of the nodes"
    , haskellTargets )
  , ( Option []    ["generic"] (NoArg (\o -> o {generic = True}))
          "Derive Data, Generic, and Typeable instances for AST types"
    , haskellTargets )
  , ( Option []    ["xml"] (NoArg (\o -> o {xml = 1}))
          "Also generate a DTD and an XML printer"
    , haskellTargets )
  , ( Option []    ["xmlt"] (NoArg (\o -> o {xml = 2}))
          "DTD and an XML printer, another encoding"
    , haskellTargets )
  -- Agda does not support the GADT syntax
  , ( Option []    ["agda"] (NoArg (\o -> o { agda = True, tokenText = TextToken }))
          "Also generate Agda bindings for the abstract syntax"
    , [TargetHaskell] )
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
  , Option ""  ["force"]     (NoArg (\ o -> o { force = True }))
      "Ignore errors in the grammar (may produce ill-formed output or crash)"
  ]
  where setMakefile mf o = o { make = Just mf }

allOptions :: [OptDescr (SharedOptions -> SharedOptions)]
allOptions = targetOptions ++ commonOptions ++ map fst specificOptions

-- | All target options and all specific options for a given target.
allOptions' :: Target -> [OptDescr (SharedOptions -> SharedOptions)]
allOptions' t = targetOptions ++ commonOptions ++ specificOptions' t

-- ~~~ Help strings ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

versionString :: String
versionString = showVersion version

title :: [String]
title =
  [ "The BNF Converter, " ++ versionString ++ " (c) 2002-today BNFC development team."
  , "Free software under the BSD 3-clause license."
  , "List of recent contributors at https://github.com/BNFC/bnfc/graphs/contributors."
  , "Report bugs at https://github.com/BNFC/bnfc/issues."
  , ""
  ]

-- oldContributors :: [String]
-- oldContributors =
--   [ "(c) Jonas Almström Duregård, Krasimir Angelov, Jean-Philippe Bernardy, Björn Bringert, Johan Broberg, Paul Callaghan, "
--   , "    Grégoire Détrez, Markus Forsberg, Ola Frid, Peter Gammie, Thomas Hallgren, Patrik Jansson, "
--   , "    Kristofer Johannisson, Antti-Juhani Kaijanaho, Ulf Norell, "
--   , "    Michael Pellauer and Aarne Ranta 2002 - 2013."
--   ]

usage :: String
usage = unlines
  [ "usage: bnfc [--TARGET] [OPTIONS] LANG.cf"
  , "   or: bnfc --[numeric-]version"
  , "   or: bnfc [--license]"
  , "   or: bnfc [--help]"
  ]

help :: String
help = unlines $ title ++
    [ usage
    , usageInfo "Global options"   globalOptions
    , usageInfo "Common options"   commonOptions
    , usageInfo "TARGET languages" targetOptions
    ] ++ map targetUsage helpTargets
  where
  helpTargets = [ TargetHaskell, TargetJava, TargetC, TargetCpp ]
  targetUsage t = usageInfo
    (printf "Special options for the %s backend" (show t))
    (specificOptions' t)

-- ~~~ Parsing machinery ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | Main parsing function
parseMode :: [String] -> (Mode, UsageWarnings)
parseMode args =
  case runWriterT $ parseMode' =<< translateOldOptions args of
    Left err  -> (UsageError err, [])
    Right res -> res

type ParseOpt = WriterT UsageWarnings (Either String)
type UsageWarnings = [String]

instance {-# OVERLAPPING #-} Semigroup (ParseOpt ()) where (<>)   = (>>)
instance {-# OVERLAPPING #-} Monoid    (ParseOpt ()) where mempty = pure (); mappend = (<>)

parseMode' :: [String] -> ParseOpt Mode
parseMode' []   = return Help
parseMode' args =
  -- First, check for global options like --help or --version
  case getOpt' Permute globalOptions args of
   (mode:_,_,_,_) -> return mode

   -- Then, check for unrecognized options.
   _ -> do
    let (_, _, unknown, _) = getOpt' Permute allOptions args
    processUnknownOptions unknown

    -- Then, determine target language.
    case getOpt' Permute targetOptions args of
      -- ([]     ,_,_,_) -> usageError "No target selected"  -- --haskell is default target
      (_:_:_,_,_,_) -> usageError "At most one target is allowed"

      -- Finally, parse options with known target.
      (optionUpdates,_,_,_) -> do
        let tgt = target (options optionUpdates)
        case getOpt' Permute (allOptions' tgt) args of
          (_,  _, _,      e:_) -> usageError e
          (_,  _, [u],      _) -> usageError $ unwords $ [ "Backend", show tgt, "does not support option", u ]
          (_,  _, us@(_:_), _) -> usageError $ unwords $ [ "Backend", show tgt, "does not support options" ] ++ us
          (_, [], _,        _) -> usageError "Missing grammar file"
          (optionsUpdates, [grammarFile], [], []) -> do
            let opts = (options optionsUpdates)
                       { lbnfFile = grammarFile
                       , lang = takeBaseName grammarFile
                       }
            warnDeprecatedBackend tgt
            warnDeprecatedOptions opts
            return $ Target opts grammarFile
          (_,  _, _,        _) -> usageError "Too many arguments"
  where
  options optionsUpdates = foldl (.) id optionsUpdates defaultOptions
  usageError = return . UsageError


-- * Deprecation

class Maintained a where
  maintained   :: a -> Bool
  printFeature :: a -> String

instance Maintained Target where
  printFeature = printTargetOption
  maintained = \case
    TargetC           -> True
    TargetCpp         -> True
    TargetCppNoStl    -> True
    TargetHaskell     -> True
    TargetHaskellGadt -> True
    TargetLatex       -> True
    TargetJava        -> True
    TargetOCaml       -> True
    TargetPygments    -> True
    TargetCheck       -> True

instance Maintained AlexVersion where
  printFeature = printAlexOption
  maintained = \case
    Alex3 -> True

instance Maintained HappyMode where
  printFeature = \case
    Standard -> undefined
    GLR      -> "--glr"
  maintained = \case
    Standard -> True
    GLR      -> False

warnDeprecatedBackend :: Maintained a => a -> ParseOpt ()
warnDeprecatedBackend backend =
  Ctrl.unless (maintained backend) $ warnDeprecated $ unwords [ "backend", printFeature backend ]

warnDeprecated :: String -> ParseOpt ()
warnDeprecated feature =
  tell
    [ unwords [ "Warning:", feature, "is deprecated and no longer maintained." ]
    -- , "Should it be broken, try an older version of BNFC."
    ]

warnDeprecatedOptions :: SharedOptions -> ParseOpt ()
warnDeprecatedOptions Options{..} = do
  warnDeprecatedBackend alexMode
  warnDeprecatedBackend glr

-- * Backward compatibility

-- | Produce a warning for former options that are now obsolete.
--   Throw an error for properly unknown options.
--
--   Note: this only works properly for former options that had no arguments.
processUnknownOptions :: [String] -> ParseOpt ()
processUnknownOptions os = do

  -- Classify unknown options.
  let cl = map (\ o -> bimap (bimap (o,) (o,)) (o,) $ classifyUnknownOption o) os
  let (errs, obsolete) = partitionEithers cl

  -- Print warnings about obsolete options.
  case map (\ (o, ObsoleteOption) -> o) obsolete of
    []       -> pure ()
    os@[_]   -> tell [ unwords $ "Warning: ignoring obsolete option:"  : os ]
    os@(_:_) -> tell [ unwords $ "Warning: ignoring obsolete options:" : os ]

  -- Throw errors.
  unless (null errs) $ do
    let (unknown, removed) = partitionEithers errs
    throwError $ unlines $ concat
      [ [ "Option error(s):" ]
      , case map (\ (o, UnknownOption) -> o) unknown of
          []       -> []
          us@[_]   -> [ unwords $ "Unrecognized option:"  : us ]
          us@(_:_) -> [ unwords $ "Unrecognized options:" : us ]
      , map (\ (o, RemovedOption msg) -> concat [ o, ": ", msg ]) removed
      ]

-- | Option has never been known.
data UnknownOption  = UnknownOption

-- | Option is obsolete, print warning and continue.
data ObsoleteOption = ObsoleteOption

-- | Error: Option has been removed, throw error with given message.
newtype RemovedOption = RemovedOption String

classifyUnknownOption :: String -> Either (Either UnknownOption RemovedOption) ObsoleteOption
classifyUnknownOption = \case
  "--alex1" -> supportRemovedIn290 $ "Alex version 1"
  "--alex2" -> supportRemovedIn290 $ "Alex version 2"
  "--alex3" -> obsolete
  s@"--sharestrings" -> optionRemovedIn290 s
  s@"--cnf" -> optionRemovedIn290 s
  "--csharp" -> supportRemovedIn290 "C#"
  "--profile" -> supportRemovedIn290 "permutation profiles"
  _ -> unknown
  where
  unknown  = Left $ Left UnknownOption
  obsolete = Right ObsoleteOption
  removed  = Left . Right . RemovedOption
  supportRemovedIn290 feature = removed $
    unwords [ "Support for", feature, removedIn290 ]
  optionRemovedIn290 o = removed $
    unwords [ "Option", o, removedIn290 ]

removedIn290 :: String
removedIn290 = "has been removed in version 2.9.0."

-- | A translation function to maintain backward compatibility
--   with the old option syntax.

translateOldOptions :: [String] -> ParseOpt [String]
translateOldOptions = mapM $ \ o -> do
   case Map.lookup o translation of
     Nothing -> return o
     Just o' -> do
       tell [ unwords [ "Warning: unrecognized option", o, "treated as if", o', "was provided." ] ]
       return o'
  where
  translation = Map.fromList $
    [ ("-agda"         , "--agda")
    , ("-java"         , "--java")
    , ("-java1.5"      , "--java")
    , ("-c"            , "--c")
    , ("-cpp"          , "--cpp")
    , ("-cpp_stl"      , "--cpp")
    , ("-cpp_no_stl"   , "--cpp-nostl")
    , ("-csharp"       , "--csharp")
    , ("-ocaml"        , "--ocaml")
    , ("-haskell"      , "--haskell")
    , ("-prof"         , "--profile")
    , ("-gadt"         , "--haskell-gadt")
    , ("-alex1"        , "--alex1")
    , ("-alex2"        , "--alex2")
    , ("-alex3"        , "--alex3")
    , ("-sharestrings" , "--sharestrings")
    , ("-bytestrings"  , "--bytestrings")
    , ("-glr"          , "--glr")
    , ("-xml"          , "--xml")
    , ("-xmlt"         , "--xmlt")
    , ("-vs"           , "--vs")
    , ("-wcf"          , "--wcf")
    , ("-generic"             , "--generic")
    , ("--ghc"                , "--generic")
    , ("--deriveGeneric"      , "--generic")
    , ("--deriveDataTypeable" , "--generic")
    ]
