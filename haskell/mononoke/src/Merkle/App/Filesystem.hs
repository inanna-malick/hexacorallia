{-# LANGUAGE TypeFamilies #-}
{-# language ScopedTypeVariables        #-}



module Merkle.App.Filesystem where


import           Merkle.App.Filesystem.Safe
import           Merkle.App.LocalState
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



getOrMakeSnapshotFT
  :: forall m
   . ( MonadError String m
     , MonadFileSystem m
     )
  => Store m
  -> m (Term (Lazy m M) 'FileTree)
getOrMakeSnapshotFT store = do
  snapshot <- getOrMakeSnapshot store
  (HC (Tagged _ snapshot')) <- fetchLMMT snapshot
  let (Snapshot ft _ _) = snapshot'
  pure $ fromLMT ft

getOrMakeSnapshot
  :: forall m
   . ( MonadError String m
     , MonadFileSystem m
     )
  => Store m
  -> m (LMMT m 'SnapshotT)
getOrMakeSnapshot store = do
  localState <- readLocalState
  currentCommit <- lsCurrentCommit localState
  case M.lookup currentCommit (snapshotMappings localState) of
    Nothing -> do
      lastCommit <- view #node $ unTerm $ lazyLoadHash store currentCommit
      let lastCommit' :: M (WIPT m) 'CommitT
            = hfmap (unmodifiedWIP . toLMT) lastCommit
      -- why does makeSnapshot require IO?
      snapshotEWIP <- runExceptT $ makeSnapshot lastCommit' (iRead nullIndex) (sRead store)
      snapshotWIP <- either (throwError . ("merge errors in history during commit op" ++) . show) pure snapshotEWIP
      snapshot <- uploadWIPT (sWrite store) $ modifiedWIP snapshotWIP

      let localState' =
            localState
              { snapshotMappings = M.insert currentCommit (hashOfLMMT snapshot) (snapshotMappings localState)
              }
      writeLocalState localState'

      pure $ snapshot
    Just snapshotHash -> do
      pure $ toLMT $ lazyLoadHash store snapshotHash



-- TODO: needs to be merge aware - maybe _just_ build WIPT?
buildCommitFromFilesystemState
  :: forall m
   . ( MonadError String m
     -- , MonadIO m
     , MonadFileSystem m
     )
  => Store m
  -> String
  -> m [Change (Term M)]
buildCommitFromFilesystemState store commitMsg = do
  -- TODO: monad state for localstate or something, only persist at end.. mb..
  localState <- readLocalState
  currentCommit <- lsCurrentCommit localState
  ft <- getOrMakeSnapshotFT store
  RootPath root <- rootPath
  diffs <- compareFilesystemToTree (pure root) $ ft
  wipCommit <- case diffs of
    [] -> throwError "attempted commit with no diffs"
    changes -> do
      let parent = toLMT $ lazyLoadHash store currentCommit
      let changes' = mapChange modifiedWIP' <$> changes
      pure $ modifiedWIP $ Commit commitMsg changes' (pure $ unmodifiedWIP parent)
  newCommitHash <- hashOfLMMT <$> uploadWIPT (sWrite store) wipCommit

  localState <- readLocalState
  -- TODO: optics, modify via fn, lol
  let localState' = localState { branches =  M.insert (currentBranch localState) newCommitHash $ branches localState}
  writeLocalState localState'

  -- NOTE: doesn't establish snapshot for new commit - should do so to confirm validity LMAO TODO

  pure diffs


-- working, could do with some polish and tests (lmao) and etc
compareFilesystemToTree
  :: forall m
   . ( MonadError String m
     -- , MonadIO m
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
