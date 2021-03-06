{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Merkle.Bonsai.MergeTrie
  ( module Merkle.Bonsai.MergeTrie,
    MergeTrie (..),
    ErrorAnnotatedMergeTrie,
    mtFilesAtPath,
    mtChildren,
    MergeError,
    Index (..),
    IndexRead,
    IndexWrite,
    stmIOIndex,
    nullIndex,
  )
where

--------------------------------------------
import Control.Monad.Except
import Data.Default
import Data.Fix (Fix (..))
import qualified Data.Foldable as Foldable
import Data.Functor.Compose
import Data.Functor.Foldable (cata, para)
import Data.List.NonEmpty (NonEmpty, nonEmpty, toList)
import Data.Map.Merge.Strict
import qualified Data.Map.Strict as Map
--------------------------------------------
import Merkle.Bonsai.MergeTrie.Index
import Merkle.Bonsai.MergeTrie.Types
import Merkle.Bonsai.Types
import Merkle.Generic.HRecursionSchemes
import Merkle.Generic.Merkle (lazyExpandHash, newStructure, oldStructure, partialUpdateHash, partialUpdateLayer)
import Optics hiding (Index)

--------------------------------------------

-- can then use other function to add commit to get snapshot
resolveMergeTrie ::
  forall m.
  Term (PartialUpdate m) 'CommitT ->
  Fix (MergeTrie m) ->
  (NonEmpty MergeError) `Either` Term (PartialUpdate m) 'FileTree
resolveMergeTrie wiptCommit mt = either (\e -> Left $ convertErrs $ cata f e []) Right x
  where
    x = resolveMergeTrie' wiptCommit mt
    convertErrs :: [MergeError] -> (NonEmpty MergeError)
    convertErrs errs = case nonEmpty errs of
      Just xs -> xs
      Nothing ->
        error "algorithm invariant broken: must be at least one error in ErrorAnnotatedMergeTrie"

    f ::
      (ErrorAnnotatedMergeTrie m) ([Path] -> [MergeError]) ->
      [Path] ->
      [MergeError]
    f (Compose (Left _)) _ = []
    f (Compose (Right (Compose (me, mt'@MergeTrie {})))) path =
      let me' = maybe [] (\e -> [ErrorAtPath path e]) me
       in Foldable.fold (g path <$> Map.toList (mtChildren mt')) ++ me'

    g ::
      forall x.
      [Path] ->
      (Path, Either x ([Path] -> [MergeError])) ->
      [MergeError]
    g path (pathSegment, enext) = either (const []) id $ (\h -> h $ path ++ [pathSegment]) <$> enext

-- can then use other function to add commit to get snapshot
resolveMergeTrie' ::
  forall m.
  Term (PartialUpdate m) 'CommitT ->
  Fix (MergeTrie m) ->
  Fix (ErrorAnnotatedMergeTrie m) `Either` Term (PartialUpdate m) 'FileTree
resolveMergeTrie' commit root = do
  mft <- para g root
  case mft of
    Nothing -> pure $ newStructure $ Dir Map.empty
    Just x -> pure $ x
  where
    g :: RAlgebra (MergeTrie m) (Fix (ErrorAnnotatedMergeTrie m) `Either` Maybe (Term (PartialUpdate m) 'FileTree))
    g mt@MergeTrie {..} = do
      let liftMT ::
            ( Fix (MergeTrie m),
              Fix (ErrorAnnotatedMergeTrie m) `Either` Maybe (Term (PartialUpdate m) 'FileTree)
            ) ->
            Fix (ErrorAnnotatedMergeTrie m)
          -- this assumes no nested further errors, I think I need to recurse here, or at least look at the snd arg of the input
          liftMT = Fix . Compose . Left . fst
          liftErr' :: Maybe MergeErrorAtPath -> Fix (ErrorAnnotatedMergeTrie m) `Either` Maybe (Term (PartialUpdate m) 'FileTree)
          liftErr' me = Left $ Fix $ Compose $ Right $ Compose (me, fmap (liftMT) mt)
          liftErr = liftErr' . Just

      let echildren = Map.toList . Map.mapMaybe id <$> traverse (either (pure . Just) snd) (mtChildren mt)
          echildren' = case echildren of
            Right x -> Right x
            -- if we hit an error in a child node, we can't just return that error annotated subtree
            -- so reconstruct the current tree with all children lifted to error annotated subtrees

            -- NOTE: UI tests show only most-nested error being displayed
            Left _ ->
              let f ::
                    ( Fix (MergeTrie m),
                      Fix (ErrorAnnotatedMergeTrie m) `Either` Maybe (Term (PartialUpdate m) 'FileTree)
                    ) ->
                    Fix (ErrorAnnotatedMergeTrie m)
                  f (t, e) = either id (const $ liftMT (t, e)) e
               in -- aha, Nothing - this is preventing errors from being seen if a nested child has an error
                  -- but how could this be fixed? do I really want to run that case match block below if children are errored out?
                  -- FIXME/TODO ^^ figure this out
                  Left $ Fix $ Compose $ Right $ Compose (Nothing, fmap f mt)

      children <- echildren'

      case ( fst . snd <$> Map.toList (mtFilesAtPath mt), -- files at path       (candidates from different merge commits)
             mtChange, -- optional change     (from currently applied commit)
             children -- child nodes at path (child nodes)
           ) of
        ([], Nothing, []) -> pure Nothing -- empty node with no files or changes - delete
        ([], Nothing, _ : _) ->
          -- TODO: more elegant matching statement for 'any nonempty'
          let children' = Map.fromList children -- no file or change but
           in pure $ Just $ newStructure $ Dir children' -- at least one child, retain
        ([], Just Del, _) -> liftErr DeleteAtNodeWithNoFile
        (fs, Just (Add blob), []) ->
          -- an add addressed to a node with any number of files - simple good state
          pure $
            Just $
              newStructure $
                File $
                  SnapshotFile
                    blob
                    commit
                    (fmap (\fh -> fh) fs)
        ([], Just (Add _), _ : _) -> liftErr AddChangeAtNodeWithChildren
        ([file], Nothing, []) ->
          pure $ Just file -- single file with no changes, simple, valid
        (_ : _, Just Del, []) -> pure Nothing -- any number of files, deleted, valid
        ((_ : _ : _), Nothing, []) ->
          -- > 1 file at same path, each w/ different hashes (b/c list converted from map)
          liftErr MoreThanOneFileButNoChange
        (_ : _, Nothing, _ : _) -> liftErr OneOrMoreFilesButWithChildren
        (_ : _, Just (Add _), _ : _) -> liftErr OneOrMoreFilesButWithChildren
        -- one or more files, but with children, but files at this path are deleted so it's valid
        (_ : _, Just Del, _ : _) ->
          let children' = Map.fromList children
           in pure $ Just $ newStructure $ Dir children'

-- | build a snapshot for a commit, recursively descending
--   into parent commits to build snapshots if neccessary
makeSnapshot ::
  Monad m =>
  -- => MonadIO m -- TODO: monad logging for this exact case
  M (Term (PartialUpdate m)) 'CommitT -> -- can provide LMMT via 'oldStructure'
  IndexRead m -> -- index, will be always Nothing for initial WIP
  StoreRead m -> -- for expanding index reads
  ExceptT (NonEmpty MergeError) m (M (Term (PartialUpdate m)) 'SnapshotT)
makeSnapshot commit index storeRead = do
  (snapshots, mt') <- case commit of
    Commit _msg changes parents -> makeMT changes parents index storeRead
    NullCommit -> pure ([], def)

  -- liftIO $ do
  --   print "mergetrie:"
  --   let lines = renderMergeTrie mt'
  --   traverse (\s -> putStr "  " >> putStrLn s) lines

  ft <- ExceptT $ pure $ resolveMergeTrie (newStructure commit) mt'
  let snap = Snapshot ft (newStructure commit) snapshots

  -- liftIO $ do
  --   print $ "done processing commit: " ++ msg
  --   print "built snapshot with ft:"
  --   let lines = renderWIPT ft
  --   traverse (\s -> putStr "  " >> putStrLn s) lines

  pure snap

-- TODO: this is, I guess, what it do (?) use this in the new App module yo

-- | build a snapshot for a commit, recursively descending
--   into parent commits to build snapshots if neccessary
makeMT ::
  Monad m =>
  -- => MonadIO m
  [Change (Term (PartialUpdate m))] -> -- list of inline changes
  NonEmpty (Term (PartialUpdate m) 'CommitT) -> -- parent commits
  IndexRead m -> -- index, will be always Nothing for initial WIP
  StoreRead m -> -- for expanding index reads
  ExceptT (NonEmpty MergeError) m ([Term (PartialUpdate m) 'SnapshotT], Fix (MergeTrie m))
makeMT changes parents index storeRead = do
  -- lines <- renderLMMT commit
  -- liftIO $ do
  --   print $ "processing commit: " ++ msg
  --   traverse (\s -> putStr "  " >> putStrLn s) lines

  let flip2 = (flip .) . flip
      e = pure ([], def)
  (snapshots, mt) <- flip2 foldl e parents $ \mstate parentCommit -> do
    (snapshots, mt) <- mstate
    lift (index (partialUpdateHash parentCommit)) >>= \case
      Just snap -> do
        snap' <- lift $ lazyExpandHash storeRead snap ^. #node
        case snap' of
          Snapshot ft _ _ -> do
            mt' <- lift $ buildMergeTrie mt (oldStructure ft)
            let wipt' = oldStructure $ lazyExpandHash storeRead snap
                snapshots' = snapshots ++ [wipt']
            pure (snapshots', mt')
      Nothing -> do
        parentCommit' <- lift $ partialUpdateLayer parentCommit
        snap <- makeSnapshot parentCommit' index storeRead
        let (Snapshot ft _ _) = snap
        mt' <- lift $ buildMergeTrie mt ft
        -- liftIO $ do
        --   print "mergetrie:"
        --   let lines = renderMergeTrie mt'
        --   traverse (\s -> putStr "  " >> putStrLn s) lines
        let wipt' = newStructure snap
            snapshots' = snapshots ++ [wipt']
        pure (snapshots', mt')

  mt' <- ExceptT $ fmap (either (Left . pure . InvalidChange) Right) $ runExceptT $ applyChanges mt changes

  pure (snapshots, mt')

-- | fold a snapshot into a mergetrie
buildMergeTrie :: forall m. Monad m => Fix (MergeTrie m) -> Term (PartialUpdate m) 'FileTree -> m (Fix (MergeTrie m))
buildMergeTrie original = para f original
  where
    f ::
      MergeTrie
        m
        ( Fix (MergeTrie m),
          Term (PartialUpdate m) 'FileTree -> m (Fix (MergeTrie m))
        ) ->
      Term (PartialUpdate m) 'FileTree ->
      m (Fix (MergeTrie m))
    f mt wipt = do
      m' <- partialUpdateLayer wipt
      case m' of
        Dir children -> do
          let wiptOnly = traverseMissing $ \_ ft -> pure $ Left ft
              presentInBoth = zipWithAMatched $ \_ ft e -> case e of
                Left ft' ->
                  -- two WIPT 'FileTree nodes, zip if different
                  if (partialUpdateHash ft == partialUpdateHash ft')
                    then do
                      -- short circuit if hash (==)
                      pure $ Left ft'
                    else do
                      -- not an elegant solution, but should be valid (FIXME/TODO: ???, revisit)
                      mt1 <- buildMergeTrie def ft
                      mt2 <- buildMergeTrie mt1 ft'
                      pure $ Right mt2
                Right (_, next) -> Right <$> next ft
              mtOnly = traverseMissing $ \_ e -> pure $ fmap fst e

          mtChildren' <-
            mergeA wiptOnly mtOnly presentInBoth children (mtChildren mt)

          let mt' =
                def & #change .~ mtChange mt
                  & #snapshotTrie % #children .~ mtChildren'
                  & #snapshotTrie % #filesAtPath .~ mtFilesAtPath mt

          pure $ Fix mt'
        File sf -> do
          let filesAtPath = Map.insert (partialUpdateHash wipt) (wipt, sf) (mtFilesAtPath mt)

          let children' = fmap fst <$> mtChildren mt
              mt' =
                def & #change .~ mtChange mt
                  & #snapshotTrie % #children .~ children'
                  & #snapshotTrie % #filesAtPath .~ filesAtPath

          pure $ Fix mt'

-- TODO: output error-annotated list of commits? eh, maybe later, not a priority
applyChanges ::
  forall m.
  Monad m =>
  Fix (MergeTrie m) ->
  [Change (Term (PartialUpdate m))] -> -- could just be 'Change Hash'
  ExceptT ApplyChangeError m (Fix (MergeTrie m))
applyChanges mt changes = foldl f (pure mt) changes
  where
    f :: ExceptT ApplyChangeError m (Fix (MergeTrie m)) -> Change (Term (PartialUpdate m)) -> ExceptT ApplyChangeError m (Fix (MergeTrie m))
    f mmt c = do
      mt' <- mmt
      applyChange mt' c

-- results in 'm' because it may need to expand the provided tree (which could afterall just be a hash)
applyChange ::
  forall m.
  Monad m =>
  Fix (MergeTrie m) ->
  Change (Term (PartialUpdate m)) -> -- could just be 'Change Hash'
  ExceptT ApplyChangeError m (Fix (MergeTrie m))
applyChange t c = para f t . toList $ _path c
  where
    f ::
      MergeTrie
        m
        ( Fix (MergeTrie m),
          [Path] -> ExceptT ApplyChangeError m (Fix (MergeTrie m))
        ) ->
      [Path] ->
      ExceptT ApplyChangeError m (Fix (MergeTrie m))
    f m (path : paths) = do
      mt' <- case Map.lookup path (mtChildren m) of
        Just (Left lmmt) -> lift $ applyChangeH lmmt paths $ _change c
        Just (Right (_, next)) -> next paths
        Nothing -> pure $ constructMT (_change c) paths

      let children' = Map.insert path (Right mt') $ fmap fst <$> mtChildren m

      pure $
        Fix $
          def & #change .~ mtChange m
            & #snapshotTrie % #children .~ children'
            & #snapshotTrie % #filesAtPath .~ mtFilesAtPath m
    f m [] = do
      change' <- case mtChange m of
        Nothing -> pure $ Just $ _change c
        Just _ -> ExceptT $ pure $ Left $ ChangeAlreadyExistsAtPath

      let children' = fmap fst <$> mtChildren m

      pure . Fix $
        def & #change .~ change'
          & #snapshotTrie % #children .~ children'
          & #snapshotTrie % #filesAtPath .~ mtFilesAtPath m

-- | results in 'm' because it may need to expand the provided tree (which could just be a hash)
applyChangeH ::
  forall m.
  Monad m =>
  Term (PartialUpdate m) 'FileTree ->
  [Path] ->
  ChangeType (Term (PartialUpdate m)) ->
  m (Fix (MergeTrie m))
applyChangeH localFileTree fullPath ct = do
  localFileTree' <- partialUpdateLayer localFileTree
  case localFileTree' of
    Dir children -> case fullPath of
      [] -> do
        -- port over dir structure
        let children' = fmap Left children
            mt =
              def & #change .~ Just ct
                & #snapshotTrie % #children .~ children'

        pure $ Fix mt
      (path : paths) -> do
        -- recurse into children -or- build new structure at new path
        children' <- case Map.lookup path children of
          Just c -> do
            child <- applyChangeH c paths ct
            pure $ Map.insert path (Right child) $ fmap Left children
          Nothing -> pure $ Map.insert path (Right $ constructMT ct paths) $ fmap Left children
        let mt = def & #snapshotTrie % #children .~ children'

        pure $ Fix mt
    File sf -> case fullPath of
      [] -> do
        let files = Map.singleton (partialUpdateHash localFileTree) (localFileTree, sf)
            mt =
              def & #change .~ Just ct
                & #snapshotTrie % #filesAtPath .~ files
        pure $ Fix mt
      (path : paths) -> do
        let children = Map.singleton path . Right $ constructMT ct paths
            files = Map.singleton (partialUpdateHash localFileTree) (localFileTree, sf)
            mt =
              def & #snapshotTrie % #children .~ children
                & #snapshotTrie % #filesAtPath .~ files
        pure $ Fix mt
