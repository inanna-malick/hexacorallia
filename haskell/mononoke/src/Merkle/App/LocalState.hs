{-# LANGUAGE TypeFamilies #-}
{-# language ScopedTypeVariables        #-}



module Merkle.App.LocalState where


import           Merkle.App.Filesystem.Safe
import           Merkle.App.Types (BranchName)
import           Merkle.Bonsai.Types hiding (Lazy, Local, PartialUpdate)

import           Control.Monad.Except
import           Data.ByteString.Lazy.UTF8 as BLU
import           Data.ByteString.UTF8 as BSU
import qualified Data.Map.Strict as M
import           Data.Aeson as AE
import           GHC.Generics



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


lsCommitForBranch
  :: MonadError String m
  => BranchName
  -> LocalState
  -> m (Hash 'CommitT)
lsCommitForBranch branchName ls = maybe (throwError "current branch does not exist in branches map") pure
                 $ M.lookup branchName (branches ls)


lsCurrentCommit
  :: MonadError String m
  => LocalState
  -> m (Hash 'CommitT)
lsCurrentCommit ls = lsCommitForBranch (currentBranch ls) ls


instance ToJSON LocalState where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON LocalState


-- set up dir with initial state
initLocalState
  :: forall m
   . ( MonadError String m
     , MonadFileSystem m
     )
  => Store m
  -> m ()
initLocalState store = do
  RootPath root <- rootPath
  let path = concatPath $ pure root <> pure localStateName
  -- check if exists
  entityTypeSafe path >>= \case
    Just DirEntity -> do
      throwError $ "state dir (???) already exists at " ++ path
    Just FileEntity -> do
      throwError $ "state file already exists at " ++ path
    Nothing -> do
      emptyCommit <- sWrite store $ NullCommit
      let state = LocalState
                { snapshotMappings   = M.empty
                , branches           = M.singleton "main" emptyCommit -- TODO: make this an option
                , currentBranch      = "main"
                }
      writeLocalState state
  pure ()

readLocalState
  :: forall m
   . ( MonadError String m
     , MonadFileSystem m
     )
  => m LocalState
readLocalState  = do
  RootPath root <- rootPath
  let path = concatPath (pure root <> pure localStateName)
  readFileSafe path >>= pure . eitherDecodeStrict . BSU.fromString  >>= liftEither


writeLocalState
  :: forall m
   . ( Monad m
     , MonadFileSystem m
     )
  => LocalState
  -> m ()
writeLocalState ls = do
  RootPath root <- rootPath
  let path = concatPath (pure root <> pure localStateName)
  writeFileSafe path $ (BLU.toString $ encode ls)

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
