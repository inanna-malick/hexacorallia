{-# LANGUAGE TypeFamilies #-}
{-# language ScopedTypeVariables        #-}



module Merkle.App where


import           Merkle.App.Command
import           Merkle.App.BackingStore
import           Merkle.App.LocalState
import           Merkle.App.Filesystem (buildCommitFromFilesystemState, compareFilesystemToTree)
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
    res <- runErrorT $ executeCommand ctx command
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
     )
  => BackingStore
  -> Command
  -> m ()
executeCommand _ctx StartGUI = liftIO mononokeGUI -- TODO: pass in port/addr
executeCommand ctx InitializeRepo = do
          root <- liftIO getCurrentDirectory
          initLocalState ctx (pure root)
executeCommand ctx (CreateCommit msg merges) = do
          root <- liftIO getCurrentDirectory
          store <- buildStoreFromCtx ctx
          -- TODO: print changes
          -- TODO: also accept merges
          changes <- buildCommitFromFilesystemState store (pure root) msg
          liftIO $ print "commit complete"
          liftIO $ print changes
executeCommand ctx ShowUncommitedChanges = do
          root <- pure <$> liftIO getCurrentDirectory
          store <- buildStoreFromCtx ctx
          localState  <- readLocalState root
          snapshot <- undefined root localState
          diffs <- compareFilesystemToTree root snapshot
          liftIO $ print diffs
executeCommand ctx (CreateBranch branchName) = do
          root <- pure <$> liftIO getCurrentDirectory
          state <- readLocalState root
          state' <- createBranchLS branchName state
          writeLocalState root state'
executeCommand _ctx (DeleteBranch branchName) = do
          root <- pure <$> liftIO getCurrentDirectory
          state  <- readLocalState root
          state' <- either throwError pure $ delBranchLS branchName state
          writeLocalState root state'
-- TODO: requires ability to impose a state on the current dir. can implement with new Filesytem.Safe
executeCommand ctx (CheckoutBranch branchName) = undefined
