{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Merkle.Bonsai.MergeTrie.Index where

--------------------------------------------
import Control.Concurrent.STM
import Control.Monad.Except
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
--------------------------------------------
import Merkle.Bonsai.Types

--------------------------------------------

type IndexRead m = Hash 'CommitT -> m (Maybe (Hash 'SnapshotT))

type IndexWrite m = Hash 'CommitT -> Hash 'SnapshotT -> m ()

data Index m = Index
  { iRead :: IndexRead m,
    iWrite :: IndexWrite m
  }

stmIOIndex :: MonadIO m => TVar (Map (Hash 'CommitT) (Hash 'SnapshotT)) -> Index m
stmIOIndex tvar =
  let index' = stmIndex tvar
   in Index
        { iRead = \h -> liftIO $ atomically $ iRead index' h,
          iWrite = \c s -> liftIO $ atomically $ iWrite index' c s
        }

stmIndex :: TVar (Map (Hash 'CommitT) (Hash 'SnapshotT)) -> Index STM
stmIndex tvar =
  Index
    { iRead = \c -> do
        bs <- readTVar tvar
        pure $ Map.lookup c bs,
      iWrite = \c s -> do
        modifyTVar tvar $ \m ->
          Map.insert c s m
        pure ()
    }

nullIndex :: Applicative m => Index m
nullIndex = Index {iRead = \_ -> pure Nothing, iWrite = \_ _ -> pure ()}
