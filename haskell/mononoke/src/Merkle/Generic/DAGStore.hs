{-# language DataKinds                  #-}
{-# language DeriveGeneric              #-}
{-# language DerivingVia                #-}
{-# language FlexibleContexts           #-}
{-# language FlexibleInstances          #-}
{-# language GADTs                      #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses      #-}
{-# language OverloadedStrings          #-}
{-# language PolyKinds                  #-}
{-# language ScopedTypeVariables        #-}
{-# language StandaloneDeriving         #-}
{-# language TemplateHaskell            #-}
{-# language TypeFamilies               #-}
{-# language TypeOperators              #-}
{-# language UndecidableInstances       #-}
{-# language OverloadedLabels           #-}
{-# language DuplicateRecordFields      #-}
{-# language DeriveAnyClass             #-}
{-# language NamedFieldPuns             #-}
{-# language RankNTypes                 #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Merkle.Generic.DAGStore
  ( GrpcClient
  , module Merkle.Generic.DAGStore
  , CanonicalForm(..)
  , Canonical.Id
  ) where

import           Mu.GRpc.Client.TyApps
import           Data.Functor.Const (Const(..))
import           Merkle.Generic.HRecursionSchemes
import           Merkle.Generic.Store
import qualified Merkle.Generic.BlakeHash as BH;
import qualified Merkle.Generic.CanonicalForm as Canonical;
import           Merkle.Generic.CanonicalForm (CanonicalForm)
import           Merkle.Generic.DAGStore.Types
import           Control.Monad.Except
import           Network.Socket (PortNumber)



mkDagStore
  :: forall m f
   . ( MonadError String m
     , MonadIO m
     , HFunctor f
     , HTraversable f
     , CanonicalForm f
     )
  => GrpcClient
  -> Store m f
mkDagStore client
  = Store
  { sRead = getM'
  , sWrite = put client
  }
  where getM
          :: NatM m BH.Hash (PartialTree f)
        getM = get client

        getM'
          :: NatM m BH.Hash (f BH.Hash)
        getM' h = getM h >>= pure . hfmap (unCxt (_tag . getHC) id)



-- with TLS disabled (via False)
mkGRPCClient :: String -> PortNumber -> GrpcClientConfig
mkGRPCClient h p = grpcClientConfigSimple h p False


defaultConfig :: GrpcClientConfig
defaultConfig = grpcClientConfigSimple "127.0.0.1" 8080 False

mkClient
  :: forall m
   . ( MonadError String m
     , MonadIO m
     )
  => GrpcClientConfig
  -> m GrpcClient
mkClient c = do
    eclient <- liftIO $ setupGrpcClient' c
    case eclient of
      Left e -> throwError $ show e
      Right client -> pure client


get
  :: forall m f
   . ( MonadError String m
     , MonadIO m
     , HTraversable f
     , CanonicalForm f
     )
  => GrpcClient
  -> NatM m BH.Hash (PartialTree f)
get client (Const h) = do
  let hash = Canonical.Hash $ BH.unpackHash' h
  liftIO $ putStrLn $ "GET node"
  response :: GRpcReply GetRespP
    <- liftIO $ gRpcCall @'MsgProtoBuf @DagStore @"DagStore" @"GetNode" client hash
  case response of
    GRpcOk g -> do
      liftIO $ putStrLn $ "GET: resp len " ++ show (length $ extra_nodes g)
      liftEither $ fromProtoGetResp $ Const g
    x -> throwError $ "GET: error response was: " ++ show x


put
  :: forall m f
   . ( MonadError String m
     , MonadIO m
     , HTraversable f
     , CanonicalForm f
     )
  => GrpcClient
  -> NatM m (f BH.Hash) BH.Hash
put client m = do
  let n = fromCanonicalNode $ Canonical.hashToId m
  liftIO $ putStrLn $ "PUT node"
  response :: GRpcReply Canonical.Hash
    <- liftIO $ gRpcCall @'MsgProtoBuf @DagStore @"DagStore" @"PutNode" client n
  case response of
    GRpcOk ph -> liftEither $ fromProtoHash $ Const ph
    x -> throwError $ "PUT: error response was: " ++ show x


