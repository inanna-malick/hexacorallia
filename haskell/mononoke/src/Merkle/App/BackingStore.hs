{-# LANGUAGE TypeFamilies #-}
{-# language ScopedTypeVariables        #-}



module Merkle.App.BackingStore where


import           Merkle.App.Types (Message, BranchName, Port, Addr)
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

import Optics
import Options.Applicative
import Data.Semigroup ((<>))


data BackingStore
  = BackingStore
  { ctxPort :: Port
  , ctxAddr :: Addr
  }

buildStoreFromCtx :: MonadError String m => MonadIO m => BackingStore -> m (Store m)
buildStoreFromCtx ctx = do
  let clientConfig = mkGRPCClient (ctxAddr ctx) (fromInteger . ctxPort $ ctx)
  client <- mkClient clientConfig
  pure $ mkDagStore client

