{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{- Backend base function. Defines the type of the backend and some usefull
 - functions -}
module BNFC.Backend.Base
  ( Backend
  , MkFiles
  , execBackend
  , mkfile
  , liftIO
  , writeFiles
  ) where

import Control.Monad.Writer
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName, (</>))

import BNFC.PrettyPrint
import BNFC.Utils (writeFileRep)

-- | Define the type of the backend functions For more purity, instead of
-- having each backend writing the generated files to disk, they return a list
-- of pairs containing the (relative) file path and the file content. This
-- allow for 1) easier testing, 2) implement common options like changing the
-- output dir or providing a diff instead of overwritting the files on a
-- highter level and 3) more purity.
--
-- The writer monad provide a more conveignent api to generate the list. Note
-- that we still use the IO monad for now because some backend insist on
-- printing stuff to the screen while generating the files.
type MkFiles a = WriterT [(FilePath, String)] IO a
type Backend = MkFiles ()


-- | Named after execWriter, this function execute the given backend
-- and returns the generated file paths and contents.
execBackend :: MkFiles () -> IO [(FilePath, String)]
execBackend = execWriterT

-- | A specialized version of tell that adds a file and its content to the
-- list of generated files
mkfile :: (FileContent c) => FilePath -> c -> MkFiles ()
mkfile path content = tell [(path, fileContentToString content)]

-- | While we are moving to generating Text.PrettyPrint.Doc instead of String,
-- it is nice to be able to use both as argument to mkfile
-- So we do some typeclass magic
class FileContent a where
    fileContentToString :: a -> String

instance FileContent [Char] where
    fileContentToString = id

instance FileContent Doc where
    fileContentToString = render

-- | Write a set of files to disk. the first argument is the root directory
-- inside which all the generated files will be written. This root directory
-- and sub-directories will be created as needed (ex: if the files contains a
-- a/b/file.txt, `writeFiles` will create the directories `$ROOT/a` and
-- `$ROOT/a/b`)
writeFiles :: FilePath -> MkFiles () -> IO ()
writeFiles root fw = do
  -- First we check that the directory exists
  fb <- execBackend fw
  createDirectoryIfMissing True root
  mapM_ (uncurry writeFile') fb
  where writeFile' :: FilePath -> String -> IO ()
        writeFile' path content =
          createDirectoryIfMissing True (root </> dropFileName path)
          >> writeFileRep (root </> path) content
