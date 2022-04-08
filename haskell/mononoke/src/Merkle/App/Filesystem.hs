{-# LANGUAGE TypeFamilies #-}
{-# language ScopedTypeVariables        #-}



module Merkle.App.Filesystem where


import           Merkle.App.Filesystem.Safe
import           Merkle.App.Types (BranchName)
import           Merkle.App.LocalState
import           Merkle.Bonsai.Types hiding (Lazy, Local, PartialUpdate)
import           Merkle.Bonsai.MergeTrie
import           Merkle.Generic.HRecursionSchemes
import           Merkle.Generic.Merkle as M

import qualified Data.Map.Merge.Strict as M
import qualified Data.Map.Strict as M
import           Control.Monad.Except
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Optics



getOrMakeSnapshotFT
  :: forall m
   . ( MonadError String m
     , MonadFileSystem m
     )
  => Store m
  -> Hash 'CommitT
  -> m (Term (Lazy m M) 'FileTree)
getOrMakeSnapshotFT store commitHash = do
  snapshot <- getOrMakeSnapshot store commitHash
  (HC (Tagged _ snapshot')) <- fetchLMMT snapshot
  let (Snapshot ft _ _) = snapshot'
  pure $ fromLMT ft

getOrMakeSnapshot
  :: forall m
   . ( MonadError String m
     , MonadFileSystem m
     )
  => Store m
  -> Hash 'CommitT
  -> m (LMMT m 'SnapshotT)
getOrMakeSnapshot store commitHash = do
  localState <- readLocalState
  case M.lookup commitHash (snapshotMappings localState) of
    Nothing -> do
      commit <- view #node $ unTerm $ lazyLoadHash store commitHash
      let commitWIP :: M (WIPT m) 'CommitT
            = hfmap (unmodifiedWIP . toLMT) commit
      snapshotWIP <- getOrMakeSnapshotWIPT store commitWIP

      commitedSnapshot <- commitSnapshot store localState commitHash snapshotWIP

      let localState' =
            localState
              { snapshotMappings = M.insert commitHash (hashOfLMMT commitedSnapshot)
                                                       (snapshotMappings localState)
              }
      writeLocalState localState'

      pure commitedSnapshot
    Just snapshotHash -> do
      pure $ toLMT $ lazyLoadHash store snapshotHash


commitSnapshot
  :: forall m
   . ( MonadError String m
     , MonadFileSystem m
     )
  => Store m
  -> LocalState
  -> Hash 'CommitT
  -> M (WIPT m) 'SnapshotT
  -> m (LMMT m 'SnapshotT)
commitSnapshot store localState commitHash snapshotWIP = do
  snapshot <- uploadWIPT (sWrite store) $ modifiedWIP snapshotWIP

  let localState' =
        localState
          { snapshotMappings = M.insert commitHash (hashOfLMMT snapshot) (snapshotMappings localState)
          }
  writeLocalState localState'
  pure snapshot



-- writes snapshot to localstate index
getOrMakeSnapshotWIPT
  :: forall m
   . ( MonadError String m
     )
  => Store m
  -> M (WIPT m) 'CommitT
  -> m (M (WIPT m) 'SnapshotT)
getOrMakeSnapshotWIPT store commit = do
      snapshotEWIP <- runExceptT $ makeSnapshot commit (iRead nullIndex) (sRead store)
      either (throwError . ("merge errors while constructing snapshot" ++) . show) pure snapshotEWIP



-- TODO: needs to be merge aware - maybe _just_ build WIPT?
buildCommitFromFilesystemState
  :: forall m
   . ( MonadError String m
     -- , MonadIO m
     , MonadFileSystem m
     )
  => Store m
  -> String
  -> [BranchName]
  -> m [Change (Term M)]
buildCommitFromFilesystemState store commitMsg branchesToMerge = do
  localState <- readLocalState
  currentCommit <- lsCurrentCommit localState
  ft <- getOrMakeSnapshotFT store currentCommit
  RootPath root <- rootPath
  diffs <- compareFilesystemToTree (pure root) $ ft
  commitsToMerge <- let mkLazy = unmodifiedWIP . toLMT . lazyLoadHash store
                        fetchBranch = fmap mkLazy . flip lsCommitForBranch localState
                     in traverse fetchBranch branchesToMerge
  commitWIP <- case diffs of
    [] -> throwError "attempted commit with no diffs"
    changes -> do
      let parent = toLMT $ lazyLoadHash store currentCommit
      let changes' = mapChange modifiedWIP' <$> changes
      pure $ Commit commitMsg changes' (unmodifiedWIP parent :| commitsToMerge)

  -- construct snapshot to ensure that new commit is valid before uploading it or updating local state
  snapshotWIP <- getOrMakeSnapshotWIPT store commitWIP
  -- after constructing valid snapshot, upload the commit
  newCommitHash <- fmap hashOfLMMT . uploadWIPT (sWrite store) $ modifiedWIP commitWIP
  -- now that we have an uploaded commit hash, write update local state
  _commitedSnapshot <- commitSnapshot store localState newCommitHash snapshotWIP


  -- read localstate again b/c it's updated in the above, TODO: mk this transactional somehow, eg state monad
  localState' <- readLocalState
  -- TODO: optics, modify via fn
  let localState'' = localState'
                   { branches =  M.insert (currentBranch localState) newCommitHash $ branches localState
                   }
  writeLocalState localState''

  pure diffs


-- working, could do with some polish and tests (lmao) and etc
compareFilesystemToTree
  :: forall m
   . ( MonadError String m
     , MonadFileSystem m
     )
  => NonEmpty Path -- not neccessarily root path, can run on subtree
  -> Term (Lazy m M) 'FileTree
  -> m [Change (Term M)]
compareFilesystemToTree root snapshot = processRoot snapshot
  where
    listDirSafe' :: Path -> m [Path]
    listDirSafe' x = filter (/= localStateName) <$> listDirSafe x
    processRoot :: Term (Lazy m M) 'FileTree -> m [Change (Term M)]
    processRoot lmmt = do
      -- TODO: logging monad
      -- liftIO $ putStrLn "processRoot"
      contents <- listDirSafe' $ concatPath root
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
      -- liftIO $ putStrLn "bothPresent"
      let absolutePath = concatPath $ root <> path
      m <- unTerm lmmt ^. #node
      case m of
        Dir  remoteDirContents  -> do
          entityTypeSafe absolutePath >>= \case
            Just FileEntity -> do
              remoteDeletes <- traverse (uncurry $ localMissing . (path <>) . pure)
                          $ M.toList remoteDirContents
              localContents <- readFileSafe absolutePath
              pure $ [Change path $ Add $ Term $ Blob localContents] ++ mconcat remoteDeletes
            Just DirEntity -> do
              contents <- listDirSafe' absolutePath
              let local = M.fromList $ fmap (\a -> (a, ())) contents
              mconcat . fmap snd . M.toList <$>
                M.mergeA (M.traverseMissing $ remoteMissing' . (path <>) . pure)
                         (M.traverseMissing $ localMissing   . (path <>) . pure)
                         (M.zipWithAMatched $ bothPresent    . (path <>) . pure)
                          local remoteDirContents
            Nothing -> do
              throwError $ "Dir replaced with non-file type entity at " ++ show path

        File remoteContentsLMMT -> do
          entityTypeSafe absolutePath >>= \case
            Just FileEntity -> do -- diff contents. potential optimization, hash local before blob fetch.
              remoteBlob <- (unTerm $ sfBlob remoteContentsLMMT) ^. #node
              let (Blob remoteContents) = remoteBlob
              localContents <- readFileSafe absolutePath
              case localContents == remoteContents of
                True -> pure [] -- no change
                False -> pure [Change path $ Add $ Term $ Blob localContents]
            Just DirEntity -> do
              contents <- listDirSafe' absolutePath
              localChanges <- mconcat <$> traverse (remoteMissing . (path <>) . pure) contents
              pure $ [Change path Del] ++ localChanges
            Nothing -> pure [] -- not a dir or a file, shrug emoji


    localMissing :: NonEmpty Path -> Term (Lazy m M) 'FileTree -> m [Change (Term M)]
    localMissing path lmmt = do
      -- liftIO $ putStrLn "localMissing"
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
      entityTypeSafe absolutePath >>= \case
        Just DirEntity  -> do
          contents <- listDirSafe' absolutePath
          mconcat <$> traverse (remoteMissing . (path <>) . pure) contents
        Just FileEntity -> do
          contents <- readFileSafe absolutePath
          pure [Change path $ Add $ Term $ Blob contents]
        Nothing -> pure [] -- not a dir or a file, shrug emoji



-- | TODO: move this out of this file?
-- checkout
--   :: forall m
--    . ( MonadFileSystem m
--      , MonadError String m
--      )
--   => Store m
--   -> BranchName
--   -> m ()
-- checkout store branchName = do
--   commitHash <- readLocalState >>= lsCommitForBranch branchName


-- -- | TODO: outside of this, add a check that there are no extant diffs - that we are exactly on _some_ commit_.
-- buildFT
--   :: forall m
--    . ( MonadIO m
--      , MonadError String m
--      )
--   => NEL.NonEmpty Path
--   -> FT
--   -> m ()
-- buildFT root ft = (cata f ft) []
--   where
--     concat path = concatPath $ maybe root (root <>) (NEL.nonEmpty path)

--     f' path (pathSegment, action) =
--       action $ path ++ [pathSegment]

--     f :: FT' ([FilePath] -> m ()) -> ([FilePath] -> m ())
--     f (Dir' xs) path = do
--       liftIO $ D.createDirectory $ concat path
--       mconcat <$> traverse (f' path) (M.toList xs)
--     f (File' _) [] = -- throw if file entity at root dir
--       throwError "file at root path of FT to build on filesystem"
--     f (File' contents) path = do
--       -- write FS state
--       liftIO $ writeFile (concat path) contents
