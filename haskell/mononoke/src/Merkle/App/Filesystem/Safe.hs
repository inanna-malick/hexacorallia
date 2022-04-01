module Merkle.App.Filesystem.Safe where

import System.FilePath.Posix
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader

import           System.Directory
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS (readFile, writeFile)
import           Data.List (isPrefixOf, isInfixOf)

class MonadFileSystem m where
  writeDirSafe  :: FilePath -> m ()
  listDirSafe   :: FilePath -> m [FilePath]
  writeFileSafe :: FilePath -> ByteString -> m ()
  readFileSafe  :: FilePath  -> m ByteString


data RootPath = RootPath FilePath

-- validation only, does not attempt to normalize. the allowed root must be an exact prefix and no '..' can be present
validatePath
  :: (MonadError String m, MonadReader RootPath m)
  => FilePath -- path to validate
  -> m ()
validatePath path = do
  (RootPath root) <- ask
  if (root `isPrefixOf` path) then pure () else throwError (mconcat ["root ", root, " is not a prefix of ", path])
  if (".." `isInfixOf` path) then pure () else throwError (mconcat [path, " contains illegal '..' sequence"])
  pure ()




instance (MonadReader RootPath m, MonadError String m, MonadIO m) => MonadFileSystem m where
  writeDirSafe path = do
    validatePath path
    -- will fail if preexisting, probably (TODO: test)
    liftIO $ createDirectory path

  listDirSafe path = do
    validatePath path
    liftIO $ listDirectory path

  writeFileSafe path contents = do
    validatePath path
    -- will fail if preexisting, probably (TODO: test)
    liftIO $ BS.writeFile path contents

  readFileSafe path = do
    validatePath path
    liftIO $ BS.readFile path








