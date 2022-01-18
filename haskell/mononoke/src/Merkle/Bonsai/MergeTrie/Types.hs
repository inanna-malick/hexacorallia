{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Merkle.Bonsai.MergeTrie.Types where


--------------------------------------------
import           Control.Concurrent.STM
import           Control.Monad.Trans
import           Control.Monad.Except
import           Data.Default
import qualified Data.Foldable as Foldable
import           Data.Functor.Compose
import qualified Data.Functor.Foldable as FF
import           Data.Functor.Foldable (para, cata)
import           Data.Fix (Fix(..))
import           Data.List.NonEmpty (NonEmpty, toList, nonEmpty)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Map.Merge.Strict
--------------------------------------------
import           Merkle.Bonsai.Types
import           Merkle.Generic.HRecursionSchemes
--------------------------------------------
import Optics hiding (Index)
import Optics.TH


-- | represents assertions from N snapshots
data SnapshotTrie m a
  = SnapshotTrie
  { -- | all files at this path
    --   using map to enforce only one entry per hash - good idea?
    -- TODO: figure out why tuple - I think b/c the process needs both list of changes and snapshot style index? idk lol
    stFilesAtPath :: Map (Hash 'FileTree)
                         (WIPT m 'FileTree, SnapshotFile (WIPT m))
  -- | a map of child entities, if any, each either a recursion
  --   or a pointer to some uncontested extant file tree entity
  , stChildren :: Map Path ((WIPT m 'FileTree) `Either` a)
  }
  deriving (Functor, Foldable, Traversable)

-- | represents assertions from N snapshots and a commit
--   used to resolve merges
data MergeTrie m a
  = MergeTrie
  { -- | Trie layer containing all assertions from snapshots
    mtSnapshotTrie :: SnapshotTrie m a
    -- | all changes at this path
  , mtChange  :: Maybe (ChangeType (WIPT m)) -- only one change per path is valid (only LMMT-only field)
  }
  deriving (Functor, Foldable, Traversable)


makeFieldLabelsFor [("stFilesAtPath", "filesAtPath"), ("stChildren", "children")] ''SnapshotTrie
makeFieldLabelsFor [("mtSnapshotTrie", "snapshotTrie"), ("mtChange", "change")] ''MergeTrie


mtChildren
  :: MergeTrie m a
  -> Map Path ((WIPT m 'FileTree) `Either` a)
mtChildren = view (#snapshotTrie % #children)

mtFilesAtPath
  :: MergeTrie m a
  -> Map (Hash 'FileTree) (WIPT m 'FileTree, SnapshotFile (WIPT m))
mtFilesAtPath = view (#snapshotTrie % #filesAtPath)

-- will add cases to enum
data MergeErrorAtPath
  = MoreThanOneFileButNoChange
  | DeleteAtNodeWithNoFile
  | AddChangeAtNodeWithChildren
  | OneOrMoreFilesButWithChildren
  deriving (Show)

data MergeError
  = ErrorAtPath [Path] MergeErrorAtPath
  | InvalidChange ApplyChangeError
  deriving (Show)

-- single layer of error annotated merge trie
type ErrorAnnotatedMergeTrie m = Either (Fix (MergeTrie m)) -- potentially a subtrie with no errors
                       `Compose` (,) (Maybe MergeErrorAtPath) -- each node potentially error tagged
                       `Compose` MergeTrie m -- the actual merge trie structure



instance Default (Fix (MergeTrie m)) where
  def = Fix def

instance Default (MergeTrie m x) where
  def = MergeTrie
      { mtChange = Nothing
      , mtSnapshotTrie = SnapshotTrie
                      { stChildren = Map.empty
                      , stFilesAtPath = Map.empty
                      }
      }



data ApplyChangeError
  = ChangeAlreadyExistsAtPath
  deriving (Show)

-- TODO move to some utils module
type RAlgebra f a = f (Fix f, a) -> a

-- | helper function, constructs merge trie with change at path
constructMT :: forall m. ChangeType (WIPT m) -> [Path] -> Fix (MergeTrie m)
constructMT change = FF.ana f
  where f :: [Path] -> MergeTrie m [Path]
        f []     = def & #change .~ Just change
        f (x:xs) = def & #snapshotTrie % #children .~ Map.singleton x (Right xs)

