{-# LANGUAGE TypeFamilies #-}

module Merkle.GUI.Core where

--------------------------------------------
import           Data.Singletons
--------------------------------------------
import           Merkle.Bonsai.Types
--------------------------------------------



data Focus x
  = SnapshotF (x 'SnapshotT)
  | FileTreeF (x 'FileTree)
  | CommitF   (x 'CommitT)
  | BlobF     (x 'BlobT)

type FocusLMMT m = Focus (LMMT m)


wrapFocus :: forall (i :: MTag) x. Sing i -> x i -> Focus x
wrapFocus s x = case s of
  SSnapshotT -> SnapshotF x
  SFileTree  -> FileTreeF x
  SCommitT   -> CommitF   x
  SBlobT     -> BlobF     x


data BranchFocus
  = MainBranch
  | OtherBranch String
  deriving (Eq, Ord, Show)
