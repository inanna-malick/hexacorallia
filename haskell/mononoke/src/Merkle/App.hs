{-# LANGUAGE TypeFamilies #-}
{-# language ScopedTypeVariables        #-}



module Merkle.App where


import           Merkle.Bonsai.Types hiding (Lazy, Local, PartialUpdate)
import           Merkle.Bonsai.MergeTrie
import           Merkle.Generic.HRecursionSchemes
import           Merkle.Generic.Merkle as M
import           Merkle.Generic.DAGStore (mkGRPCClient, mkClient)

import           System.Directory
import qualified Data.Map.Merge.Strict as M
import qualified Data.Map.Strict as M
import           Control.Monad.Except
import           Data.List.NonEmpty (NonEmpty)
import           Data.Aeson as AE
import GHC.Generics

import           Merkle.GUI.App (mononokeGUI)
import Optics
import Options.Applicative
import Data.Semigroup ((<>))

data CommandContext
  = CommandContext
  { ctxPort :: Port
  , ctxAddr :: Addr
  }

appCommand :: Parser (CommandContext, Command)
appCommand = (,) <$> ctx <*> cmd
  where
    ctx = CommandContext
      <$> option auto
          ( long "port"
         <> metavar "Port"
         <> showDefault
         <> value 8083
         <> help "DAG store port" )
      <*> strOption
          ( long "addr"
         <> metavar "Addr"
         <> showDefault
         <> value "localhost"
         <> help "DAG store address" )

    cmd = subparser
      ( command "diff" (info (pure ShowUncommitedChanges) ( progDesc "show uncommited changes"))
     <> command "commit" (info undefined ( progDesc "commit changes" ))
     <> command "create_branch" (info undefined ( progDesc "create a new branch"))
     <> command "delete_branch" (info undefined ( progDesc "delete an existing branch"))
     <> command "checkout" (info undefined
                             ( progDesc "checkout an existing branch. fails if uncommited changes")
                           )
     <> command "init" (info undefined ( progDesc "init a new repo in current dir"))
     <> command "gui" (info undefined ( progDesc "launch gui for repo" ))
      )



empty :: Applicative m => LMMT m 'FileTree
empty = liftLMMT $ Term $ Dir M.empty



-- diff: show extant changes (basically just commit dry run, super easy kinda )
type Addr = String
type Message = String
type BranchName = String
type Port = Integer
-- all commands except for Init must be run in the root dir of an initialized repo
data Command
  = StartGUI
  | InitializeRepo -- init repo in current dir
  | CreateCommit Message [BranchName] -- commit all diffs in current dir, merging the provided branches if any
  | ShowUncommitedChanges -- show diffs in current dir
  | CreateBranch BranchName-- create a branch
  | DeleteBranch BranchName-- create a branch
  | CheckoutBranch BranchName -- checkout an existing branch, imposing it on the current repo
-- NOTE: no need for snapshot browser/blame/etc, that's all via the web UI

executeCommand
  :: forall m
   . ( MonadError String m
     , MonadIO m
     )
  => CommandContext
  -> Command
  -> m ()
executeCommand ctx StartGUI = liftIO mononokeGUI
executeCommand ctx InitializeRepo = do
          currentDir <- liftIO getCurrentDirectory
          initLocalState (ctxAddr ctx) (ctxPort ctx) (pure currentDir)
executeCommand ctx (CreateCommit msg merges) = do
          currentDir <- liftIO getCurrentDirectory
          _changes <- commit (pure currentDir) msg
          pure ()
executeCommand ctx ShowUncommitedChanges = undefined
executeCommand ctx (CreateBranch branchName) = do
          currentDir <- pure <$> liftIO getCurrentDirectory
          state <- readLocalState currentDir
          state' <- checkoutBranch branchName state
          writeLocalState currentDir state'
executeCommand ctx (DeleteBranch branchName) = do
          currentDir <- pure <$> liftIO getCurrentDirectory
          state <- readLocalState currentDir
          state' <- either throwError pure $ delBranch branchName state
          writeLocalState currentDir state'
executeCommand ctx (CheckoutBranch branchName) = undefined

-- TODO: fn that checks out a branch but only if there are no diffs

-- branch off current commit
checkoutBranch :: MonadError String m => String -> LocalState -> m LocalState
checkoutBranch name ls = do
    currentCommit <- lsCurrentCommit ls
    case M.member name (branches ls) of
      False -> pure $ ls { branches = M.insert name currentCommit (branches ls) }
      True  -> throwError $ "mk branch that already exists " ++ name

-- delete existing branch
delBranch :: String -> LocalState -> Either String LocalState
delBranch name ls = case M.member name (branches ls) of
  -- TODO: error if current branch
  False -> Right $ ls { branches = M.delete name $ branches ls }
  True  -> Left $ "del branch " ++ name ++ " that doesn't exist"



-- can just require cmd line invocation to be in root dir of repo, not in a subdir
-- state store: local staged, as json, also presence of a file signifies that it's a mononoke root
-- if this file is _not_ present for anything other than init, it's an error.
data LocalState
  = LocalState
  { backingStoreAddr   :: String
  , backingStorePort   :: Integer
  , currentBranch      :: String -- INVARIANT: must exist in branches map
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
  => Addr
  -> Port
  -> NonEmpty Path
  -> m ()
initLocalState addr port path = do
  let path' = concatPath $ path <> pure localStateName
  -- check if exists
  isFile <- liftIO $ doesFileExist path'
  case isFile of
    True -> do
      throwError $ "state file already exists at " ++ path'
    False -> do
      let clientConfig = mkGRPCClient addr (fromInteger port)
      client <- mkClient clientConfig
      let store = mkDagStore client
      emptyCommit <- sWrite store $ NullCommit
      let state = LocalState
                { backingStoreAddr   = addr
                , backingStorePort   = port
                , snapshotMappings   = M.empty
                , branches           = M.singleton "main" emptyCommit
                , currentBranch      = "main"
                }
      writeLocalState path state
  pure ()


localStateName :: Path
localStateName = ".bonsai.state"


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


-- TODO: needs to be merge aware - maybe _just_ build WIPT?
commit
  :: forall m
   . ( MonadError String m
     , MonadIO m
     )
  => NonEmpty Path
  -> String
  -> m [Change (Term M)]
commit root commitMsg = do
  localState <- readLocalState root
  let clientConfig = mkGRPCClient (backingStoreAddr localState) (fromInteger $ backingStorePort localState)
  client <- mkClient clientConfig
  let store = mkDagStore client
  currentCommit <- lsCurrentCommit localState
  snapshot <- case M.lookup currentCommit (snapshotMappings localState) of
    Nothing -> do
      lastCommit <- view #node $ unTerm $ lazyLoadHash store currentCommit
      let lastCommit' :: M (WIPT m) 'CommitT
            = hfmap (unmodifiedWIP . toLMT) lastCommit
      snapshotEWIP <- runExceptT $ makeSnapshot lastCommit' (iRead nullIndex) (sRead store)
      snapshotWIP <- either (throwError . ("merge errors in history during commit op" ++) . show) pure snapshotEWIP
      snapshot <- uploadWIPT (sWrite store) $ modifiedWIP snapshotWIP
      pure $ snapshot
    Just snapshotHash -> do
      pure $ toLMT $ lazyLoadHash store snapshotHash
  (HC (Tagged _ snapshot')) <- fetchLMMT snapshot
  let (Snapshot ft _ _) = snapshot'
  diffs <- diffLocalState root $ fromLMT ft
  wipCommit <- case diffs of
    [] -> throwError "attempted commit with no diffs"
    changes -> do
      let parent = toLMT $ lazyLoadHash store currentCommit
      let changes' = mapChange modifiedWIP' <$> changes
      pure $ modifiedWIP $ Commit commitMsg changes' (pure $ unmodifiedWIP parent)
  newCommitHash <- hashOfLMMT <$> uploadWIPT (sWrite store) wipCommit

  -- TODO: optics, modify via fn, lol
  let localState' = localState { branches =  M.insert (currentBranch localState) newCommitHash $ branches localState}
  writeLocalState root localState'

  -- NOTE: doesn't establish snapshot for new commit - should do so to confirm validity LMAO TODO

  pure diffs


-- working, could do with some polish and tests (lmao) and etc
diffLocalState
  :: forall m
   . ( MonadError String m
     , MonadIO m
     )
  => NonEmpty Path
  -> Term (Lazy m M) 'FileTree
  -> m [Change (Term M)]
diffLocalState root snapshot = processRoot snapshot
  where
    listDirectory' x = filter (/= localStateName) <$> listDirectory x
    processRoot :: Term (Lazy m M) 'FileTree -> m [Change (Term M)]
    processRoot lmmt = do
      liftIO $ putStrLn "processRoot"
      contents <- liftIO $ listDirectory' $ concatPath root
      let local = M.fromList $ fmap (\a -> (a, ())) contents
      remote <- do
        m <- unTerm lmmt ^. #node
        case m of
          File _ -> throwError "expected root path to be a dir in LMMT"
          Dir  d -> pure  d
      mconcat . fmap snd . M.toList <$>
        M.mergeA (M.traverseMissing $ remoteMissing' . pure)
                 (M.traverseMissing $ localMissing   . pure)
                 (M.zipWithAMatched $ bothPresent    . pure)
                  local remote


    -- both paths present, file/dir conflict still possible
    bothPresent :: NonEmpty Path -> () -> Term (Lazy m M) 'FileTree -> m [Change (Term M)]
    bothPresent path () lmmt = do
      liftIO $ putStrLn "bothPresent"
      let absolutePath = concatPath $ root <> path
      m <- unTerm lmmt ^. #node
      case m of
        Dir  remoteDirContents  -> do
          isDir <- liftIO $ doesDirectoryExist absolutePath
          case isDir of
            False -> do -- remote dir, local ???, if file then add/remote del, else ???
              isFile <- liftIO $ doesFileExist absolutePath
              case isFile of
                False -> throwError $ "Dir replaced with non-file type entity at " ++ show path
                True  -> do -- remote dir, local file
                  remoteDeletes <- traverse (uncurry $ localMissing . (path <>) . pure)
                             $ M.toList remoteDirContents
                  localContents <- liftIO $ readFile absolutePath
                  pure $ [Change path $ Add $ Term $ Blob localContents] ++ mconcat remoteDeletes

            True  -> do
              contents <- liftIO $ listDirectory' absolutePath
              let local = M.fromList $ fmap (\a -> (a, ())) contents
              mconcat . fmap snd . M.toList <$>
                M.mergeA (M.traverseMissing $ remoteMissing' . (path <>) . pure)
                         (M.traverseMissing $ localMissing   . (path <>) . pure)
                         (M.zipWithAMatched $ bothPresent    . (path <>) . pure)
                          local remoteDirContents

        File remoteContentsLMMT -> do
          isFile <- liftIO $ doesFileExist absolutePath
          case isFile of
            True -> do -- diff contents. potential optimization, hash local before blob fetch.
              remoteBlob <- (unTerm $ sfBlob remoteContentsLMMT) ^. #node
              let (Blob remoteContents) = remoteBlob
              localContents <- liftIO $ readFile absolutePath
              case localContents == remoteContents of
                True -> pure [] -- no change
                False -> pure [Change path $ Add $ Term $ Blob localContents]
            False -> do
              localChanges <- do
                isDir <- liftIO $ doesDirectoryExist absolutePath
                case isDir of
                  False -> pure [] -- not a dir or a file, shrug emoji
                  True  -> do
                    contents <- liftIO $ listDirectory' absolutePath
                    mconcat <$> traverse (remoteMissing . (path <>) . pure) contents
              pure $ [Change path Del] ++ localChanges



    localMissing :: NonEmpty Path -> Term (Lazy m M) 'FileTree -> m [Change (Term M)]
    localMissing path lmmt = do
      liftIO $ putStrLn "localMissing"
      m <- unTerm lmmt ^. #node
      case m of
        Dir  remoteDirContents -> do
          res <- traverse (uncurry $ localMissing . (path <>) . pure) $ M.toList remoteDirContents
          pure $ mconcat res
        File _ -> pure [Change path Del]


    remoteMissing' :: NonEmpty Path -> () -> m [Change (Term M)]
    remoteMissing' path () = remoteMissing path

    remoteMissing :: NonEmpty Path -> m [Change (Term M)]
    remoteMissing path = do
      let absolutePath = concatPath $ root <> path
      isFile <- liftIO $ doesFileExist absolutePath
      case isFile of
        True -> do
          contents <- liftIO $ readFile absolutePath
          pure [Change path $ Add $ Term $ Blob contents]
        False -> do
          isDir <- liftIO $ doesDirectoryExist absolutePath
          case isDir of
            False -> do
              pure [] -- not a dir or a file, shrug emoji
            True  -> do
              contents <- liftIO $ listDirectory' absolutePath
              mconcat <$> traverse (remoteMissing . (path <>) . pure) contents

