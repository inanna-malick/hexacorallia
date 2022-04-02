{-# LANGUAGE TypeFamilies #-}
{-# language ScopedTypeVariables        #-}



module Merkle.App where


import           Merkle.App.Filesystem.Safe (RootPath(..), MonadFileSystem(..))
import           Merkle.App.Command
import           Merkle.App.BackingStore
import           Merkle.App.LocalState
import           Merkle.App.Filesystem (getOrMakeSnapshotFT, buildCommitFromFilesystemState, compareFilesystemToTree)
import           Merkle.Bonsai.Types hiding (Lazy, Local, PartialUpdate)
import           Merkle.Bonsai.MergeTrie
import           Merkle.Generic.HRecursionSchemes
import           Merkle.Generic.Merkle as M
import           Merkle.Generic.DAGStore (mkGRPCClient, mkClient)

import           System.Directory
import qualified Data.Map.Merge.Strict as M
import qualified Data.Map.Strict as M
import           Control.Monad.Except
import           Control.Monad.Error
import           Control.Monad.Reader (runReaderT, MonadReader)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Aeson as AE
import GHC.Generics

import           Merkle.GUI.App (mononokeGUI)
import Optics
import Options.Applicative
import Data.Semigroup ((<>))

exec :: IO ()
exec = do
    (ctx, command) <- execParser opts
    root <- RootPath <$> getCurrentDirectory
    res <- liftIO $ flip runReaderT root $ runErrorT $ do
      store <- buildStoreFromCtx ctx
      executeCommand store command
    print res

  where
    opts = info appCommand
      ( fullDesc
     <> progDesc "perform version control operations"
     <> header "a bonsai version control system based on generic merkle graph utils" )


executeCommand
  :: forall m
   . ( MonadError String m
     , MonadIO m
     , MonadReader RootPath m
     )
  => Store m
  -> Command
  -> m ()
executeCommand _store StartGUI = liftIO mononokeGUI -- TODO: pass in store
executeCommand store InitializeRepo = initLocalState store
executeCommand store (CreateCommit msg merges) = do
          -- TODO: print changes
          -- TODO: also accept merges
          changes <- buildCommitFromFilesystemState store msg
          liftIO $ print "commit complete"
          liftIO $ print changes
executeCommand store ShowUncommitedChanges = do
          snapshotFT <- readLocalState >>= lsCurrentCommit >>= getOrMakeSnapshotFT store
          RootPath root <- rootPath
          diffs <- compareFilesystemToTree (pure root) snapshotFT
          liftIO $ print diffs
executeCommand _store (CreateBranch branchName) = do
          state <- readLocalState
          state' <- createBranchLS branchName state
          writeLocalState state'
executeCommand _store (DeleteBranch branchName) = do
          state  <- readLocalState
          state' <- either throwError pure $ delBranchLS branchName state
          writeLocalState state'
-- TODO: requires ability to impose a state on the current dir. can implement with new Filesytem.Safe
executeCommand store (CheckoutBranch branchName) = undefined
