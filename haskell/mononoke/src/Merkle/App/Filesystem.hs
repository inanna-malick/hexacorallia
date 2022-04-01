{-# LANGUAGE TypeFamilies #-}
{-# language ScopedTypeVariables        #-}



module Merkle.App.Filesystem where


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


getOrMakeSnapshot
  :: forall m
   . ( MonadError String m
     , MonadIO m
     )
  => Store m
  -> NonEmpty Path
  -> m (LMMT m 'SnapshotT)
getOrMakeSnapshot store root = do
  localState <- readLocalState root
  currentCommit <- lsCurrentCommit localState
  case M.lookup currentCommit (snapshotMappings localState) of
    Nothing -> do
      lastCommit <- view #node $ unTerm $ lazyLoadHash store currentCommit
      let lastCommit' :: M (WIPT m) 'CommitT
            = hfmap (unmodifiedWIP . toLMT) lastCommit
      snapshotEWIP <- runExceptT $ makeSnapshot lastCommit' (iRead nullIndex) (sRead store)
      snapshotWIP <- either (throwError . ("merge errors in history during commit op" ++) . show) pure snapshotEWIP
      snapshot <- uploadWIPT (sWrite store) $ modifiedWIP snapshotWIP

      let localState' =
            localState
              { snapshotMappings = M.insert currentCommit (hashOfLMMT snapshot) (snapshotMappings localState)
              }
      writeLocalState root localState'

      pure $ snapshot
    Just snapshotHash -> do
      pure $ toLMT $ lazyLoadHash store snapshotHash



-- TODO: needs to be merge aware - maybe _just_ build WIPT?
buildCommitFromFilesystemState
  :: forall m
   . ( MonadError String m
     , MonadIO m
     )
  => Store m
  -> NonEmpty Path
  -> String
  -> m [Change (Term M)]
buildCommitFromFilesystemState store root commitMsg = do
  -- TODO: monad state for localstate or something, only persist at end.. mb..
  localState <- readLocalState root
  currentCommit <- lsCurrentCommit localState
  snapshot <- getOrMakeSnapshot store root
  (HC (Tagged _ snapshot')) <- fetchLMMT snapshot
  let (Snapshot ft _ _) = snapshot'
  diffs <- compareFilesystemToTree root $ fromLMT ft
  wipCommit <- case diffs of
    [] -> throwError "attempted commit with no diffs"
    changes -> do
      let parent = toLMT $ lazyLoadHash store currentCommit
      let changes' = mapChange modifiedWIP' <$> changes
      pure $ modifiedWIP $ Commit commitMsg changes' (pure $ unmodifiedWIP parent)
  newCommitHash <- hashOfLMMT <$> uploadWIPT (sWrite store) wipCommit

  localState <- readLocalState root
  -- TODO: optics, modify via fn, lol
  let localState' = localState { branches =  M.insert (currentBranch localState) newCommitHash $ branches localState}
  writeLocalState root localState'

  -- NOTE: doesn't establish snapshot for new commit - should do so to confirm validity LMAO TODO

  pure diffs


-- working, could do with some polish and tests (lmao) and etc
compareFilesystemToTree
  :: forall m
   . ( MonadError String m
     , MonadIO m
     )
  => NonEmpty Path
  -> Term (Lazy m M) 'FileTree
  -> m [Change (Term M)]
compareFilesystemToTree root snapshot = processRoot snapshot
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

