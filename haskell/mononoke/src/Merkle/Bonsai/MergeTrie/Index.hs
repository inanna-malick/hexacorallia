{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Merkle.Bonsai.MergeTrie.Index where


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


type IndexRead m  = Hash 'CommitT -> m (Maybe (Hash 'SnapshotT))
type IndexWrite m = Hash 'CommitT -> Hash 'SnapshotT -> m ()

data Index m
  = Index
  { iRead  :: IndexRead m
  , iWrite :: IndexWrite m
  }

stmIOIndex :: MonadIO m => TVar (Map (Hash 'CommitT) (Hash 'SnapshotT)) -> Index m
stmIOIndex tvar
  = let index' = stmIndex tvar
     in Index
  { iRead = \h    -> liftIO $ atomically $ iRead index' h
  , iWrite = \c s -> liftIO $ atomically $ iWrite index' c s
  }


stmIndex :: TVar (Map (Hash 'CommitT) (Hash 'SnapshotT)) -> Index STM
stmIndex tvar
  = Index
  { iRead = \c -> do
      bs <- readTVar tvar
      pure $ Map.lookup c bs
  , iWrite = \c s -> do
      modifyTVar tvar $ \m ->
        Map.insert c s m
      pure ()
  }

nullIndex :: Applicative m => Index m
nullIndex = Index { iRead = \_ -> pure Nothing, iWrite = \_ _ -> pure () }


