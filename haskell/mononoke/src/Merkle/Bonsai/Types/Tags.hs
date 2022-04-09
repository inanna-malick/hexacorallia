{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}


module Merkle.Bonsai.Types.Tags where

--------------------------------------------
import           Data.Singletons.TH
import           Data.Text (Text)
import qualified Data.Text as T
--------------------------------------------


$(singletons [d|
  data MTag = SnapshotT | FileTree | CommitT | BlobT | RepoT
 |])


typeTagFAIcon' :: forall (i :: MTag) x. SingI i => x i -> String
typeTagFAIcon' _ = typeTagFAIcon (sing @i)

typeTagFAIcon :: forall (i :: MTag). Sing i -> String
typeTagFAIcon s = case s of
  SSnapshotT -> "fa-database"
  SFileTree  -> "fa-folder-open"
  SCommitT   -> "fa-history"
  SBlobT     -> "fa-file"
  SRepoT      -> "fa-boxes-stacked"


typeTagName' :: forall (i :: MTag) x. SingI i => x i -> String
typeTagName' _ = typeTagName (sing @i)

typeTagName :: forall (i :: MTag). Sing i -> String
typeTagName s = case s of
  SSnapshotT -> "snapshot"
  SFileTree  -> "filetree"
  SCommitT   -> "commit"
  SBlobT     -> "blob"
  SRepoT     -> "repo"

