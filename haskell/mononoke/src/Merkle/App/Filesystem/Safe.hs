module Merkle.App.Filesystem.Safe where

import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.Reader (ask, MonadReader)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           System.Directory
import           Data.List (isPrefixOf, isInfixOf)


-- NOTE: operates on strings b/c mononoke merkle structure requires strings.
--       Makes it easier for humans to read encoded objects, which is a core ergonomics goal
-- safe filesystem access
class MonadFileSystem m where
  writeDirSafe   :: FilePath -> m ()
  listDirSafe    :: FilePath -> m [FilePath]
  writeFileSafe  :: FilePath -> String -> m ()
  readFileSafe   :: FilePath  -> m String
  entityTypeSafe :: FilePath -> m (Maybe FSEntityType)
  rootPath       :: m RootPath

data FSEntityType = FileEntity | DirEntity

data RootPath = RootPath FilePath

-- validation only, does not attempt to normalize. the allowed root must be an exact prefix and no '..' can be present
validatePath
  :: (MonadError String m, MonadReader RootPath m)
  => FilePath -- path to validate
  -> m ()
validatePath path = do
  (RootPath root) <- ask
  if (root `isPrefixOf` path) then pure () else throwError (mconcat ["root ", root, " is not a prefix of ", path])
  if (".." `isInfixOf` path) then throwError (mconcat [path, " contains illegal '..' sequence"]) else pure ()
  pure ()




instance (MonadReader RootPath m, MonadError String m, MonadIO m) => MonadFileSystem m where
  rootPath = ask

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
    liftIO $ writeFile path contents

  readFileSafe path = do
    validatePath path
    liftIO $ readFile path

  entityTypeSafe path = do
    validatePath path
    isDir <- liftIO $ doesDirectoryExist path
    case isDir of
      True -> pure $ Just DirEntity
      False -> do
        isFile <- liftIO $ doesFileExist path
        case isFile of
          True -> pure $ Just FileEntity
          False -> do
            -- TODO: throw if unsupported, maybe? explicit case for symlink.
            -- TODO: throwError $ mconcat ["path ", path, " is neither a file nor a directory"]
            pure Nothing
