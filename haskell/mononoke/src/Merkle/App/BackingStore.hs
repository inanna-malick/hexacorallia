{-# LANGUAGE TypeFamilies #-}
{-# language ScopedTypeVariables        #-}



module Merkle.App.BackingStore where


import           Merkle.App.Types (Port, Addr)
import           Merkle.Bonsai.Types hiding (Lazy, Local, PartialUpdate)
import           Merkle.Generic.DAGStore (mkGRPCClient, mkClient)

import           Control.Monad.Except


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

