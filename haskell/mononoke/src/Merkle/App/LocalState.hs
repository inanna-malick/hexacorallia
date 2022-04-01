{-# LANGUAGE TypeFamilies #-}
{-# language ScopedTypeVariables        #-}



module Merkle.App.LocalState where


import           Merkle.App.BackingStore (BackingStore(..))
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


localStateName :: Path
localStateName = ".bonsai.state"

-- | local state store, stored as json. presence of the local state file signifies that
-- it's a mononoke root required for all commands except for 'init'
data LocalState
  = LocalState
  { currentBranch      :: String -- INVARIANT: must exist in branches map
  -- TODO: this should be remote and fetched as needed, but the hash -> hash mapping isn't on the backend yet
  -- TODO: wait a minute, what if we made the mapping itself a hashed merkle store component
  , snapshotMappings   :: M.Map (Hash 'CommitT) (Hash 'SnapshotT)
  , branches           :: M.Map String (Hash 'CommitT)
  } deriving (Ord, Eq, Show, Generic)


lsCurrentCommit
  :: MonadError String m
  => LocalState
  -> m (Hash 'CommitT)
lsCurrentCommit ls = maybe (throwError "current branch does not exist in branches map") pure
                 $ M.lookup (currentBranch ls) (branches ls)


instance ToJSON LocalState where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON LocalState


-- set up dir with initial state
initLocalState
  :: forall m
   . ( MonadError String m
     , MonadIO m
     )
  => BackingStore
  -> NonEmpty Path
  -> m ()
initLocalState ctx path = do
  let path' = concatPath $ path <> pure localStateName
  -- check if exists
  isFile <- liftIO $ doesFileExist path'
  case isFile of
    True -> do
      throwError $ "state file already exists at " ++ path'
    False -> do
      let clientConfig = mkGRPCClient (ctxAddr ctx) (fromInteger . ctxPort $ ctx)
      client <- mkClient clientConfig
      let store = mkDagStore client
      emptyCommit <- sWrite store $ NullCommit
      let state = LocalState
                { snapshotMappings   = M.empty
                , branches           = M.singleton "main" emptyCommit -- TODO: make this an option
                , currentBranch      = "main"
                }
      writeLocalState path state
  pure ()

readLocalState
  :: forall m
   . ( MonadError String m
     , MonadIO m
     )
  => NonEmpty Path
  -> m LocalState
readLocalState containingDir = do
  let path = concatPath (containingDir <> pure localStateName)
  liftIO (eitherDecodeFileStrict path) >>= liftEither


writeLocalState
  :: forall m
   . ( MonadIO m
     )
  => NonEmpty Path
  -> LocalState
  -> m ()
writeLocalState containingDir ls = do
  let path = concatPath (containingDir <> pure localStateName)
  liftIO $ encodeFile path ls

-- branch off current commit
createBranchLS :: MonadError String m => String -> LocalState -> m LocalState
createBranchLS name ls = do
    currentCommit <- lsCurrentCommit ls
    case M.member name (branches ls) of
      False -> pure $ ls { branches = M.insert name currentCommit (branches ls) }
      True  -> throwError $ "mk branch that already exists " ++ name

-- delete existing branch
delBranchLS :: String -> LocalState -> Either String LocalState
delBranchLS name ls = do
  case currentBranch ls == name of
    True -> throwError "attempted to delete current branch, not allowed"
    False -> pure ()
  case M.member name (branches ls) of
    False -> Right $ ls { branches = M.delete name $ branches ls }
    True  -> Left $ "del branch " ++ name ++ " that doesn't exist"