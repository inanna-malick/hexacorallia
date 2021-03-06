{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Merkle.App where

import Control.Monad.Except
import Control.Monad.Reader (ReaderT, runReaderT)
import Merkle.App.BackingStore
import Merkle.App.Command
import Merkle.App.Filesystem (buildCommitFromFilesystemState, compareFilesystemToTree, getOrMakeSnapshotFT)
import Merkle.App.Filesystem.Safe (MonadFileSystem (..), RootPath (..))
import Merkle.App.LocalState
import Merkle.Bonsai.Types hiding (Lazy, Local, PartialUpdate)
import Merkle.GUI.App (mononokeGUI)
import Merkle.Generic.Store (transformStore)
import Options.Applicative (execParser, fullDesc, header, info, progDesc)
import System.Directory (getCurrentDirectory)

exec :: IO ()
exec = do
  (ctx, command) <- execParser opts
  root <- RootPath <$> getCurrentDirectory
  res <- liftIO $
    flip runReaderT root $
      runExceptT $ do
        store <- buildStoreFromCtx ctx
        executeCommand store command
  print res
  where
    opts =
      info
        appCommand
        ( fullDesc
            <> progDesc "perform version control operations"
            <> header "a bonsai version control system based on generic merkle graph utils"
        )

type CommandMonad = (ExceptT String (ReaderT RootPath IO))

-- we need to run our store in UI, so pull out the error handling and RootPath monadreader and get it in just some MonadIO
downcastStore ::
  forall m.
  MonadIO m =>
  Store CommandMonad ->
  CommandMonad (Store m)
downcastStore store = do
  r <- rootPath

  let f :: forall x. CommandMonad x -> m x
      f = liftIO . (>>= either fail pure) . flip runReaderT r . runExceptT

  -- TODO: note that it just nukes everything if there's an error, via 'fail' in IO
  pure $ transformStore f store

executeCommand ::
  Store CommandMonad ->
  Command ->
  CommandMonad ()
executeCommand store StartGUI = do
  localstate <- readLocalState
  store' <- downcastStore store
  liftIO (mononokeGUI localstate store')
executeCommand store InitializeRepo = initLocalState store
executeCommand store (CreateCommit msg merges) = do
  -- TODO: print changes
  changes <- buildCommitFromFilesystemState store msg merges
  liftIO $ putStrLn "commit complete"
  liftIO $ print changes
executeCommand store ShowUncommitedChanges = do
  -- TODO: why does this run PUT ops? must be the snapshot bit
  snapshotFT <- readLocalState >>= lsCurrentCommit >>= getOrMakeSnapshotFT store
  RootPath root <- rootPath
  diffs <- compareFilesystemToTree (pure root) snapshotFT
  liftIO $ print diffs
executeCommand _store (CreateBranch branchName) = do
  state <- readLocalState
  state' <- createBranchLS branchName state
  writeLocalState state'
executeCommand _store (DeleteBranch branchName) = do
  state <- readLocalState
  state' <- either throwError pure $ delBranchLS branchName state
  writeLocalState state'
-- TODO: requires ability to impose a state on the current dir. can implement with new Filesytem.Safe
executeCommand store (CheckoutBranch branchName) = undefined store branchName
