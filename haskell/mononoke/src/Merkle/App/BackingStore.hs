{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Merkle.App.BackingStore where

import Control.Monad.Except
import Merkle.App.Types (Addr, Port)
import Merkle.Bonsai.Types hiding (Lazy, Local, PartialUpdate)
import Merkle.Generic.DAGStore (mkClient, mkGRPCClient)

data BackingStore = BackingStore
  { ctxPort :: Port,
    ctxAddr :: Addr
  }

buildStoreFromCtx :: MonadError String m => MonadIO m => BackingStore -> m (Store m)
buildStoreFromCtx ctx = do
  let clientConfig = mkGRPCClient (ctxAddr ctx) (fromInteger . ctxPort $ ctx)
  client <- mkClient clientConfig
  pure $ mkDagStore client
