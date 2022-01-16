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
  , CanonicalEncoding(..)
  , Id
  ) where

import qualified Data.Aeson
import           Data.Kind (Type)
import           GHC.Generics
import           Mu.Quasi.GRpc
import           Mu.Schema hiding (Term(..))
import           Mu.GRpc.Client.TyApps
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Const (Const(..))
import           Merkle.Generic.HRecursionSchemes
import           Merkle.Generic.Store
import           Data.Singletons.TH
import           Data.Word
import qualified Control.Monad.State as S
import qualified Merkle.Generic.BlakeHash as BH;
import           Merkle.Generic.DAGStore.Types
import           Data.Functor.Identity
import           Data.Functor.Classes
import           Data.ByteString.Builder as BB (word32LE, toLazyByteString)
import           Control.Monad.Except
import           Network.Socket (PortNumber)


mkDagStore
  :: forall m f
   . ( MonadError String m
     , MonadIO m
     , HFunctor f
     , HTraversable f
     , CanonicalEncoding f
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
     , CanonicalEncoding f
     )
  => GrpcClient
  -> NatM m BH.Hash (PartialTree f)
get client (Const h) = do
  let hash = Hash $ BH.unpackHash' h
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
     , CanonicalEncoding f
     )
  => GrpcClient
  -> NatM m (f BH.Hash) BH.Hash
put client m = do
  let n = unrequireFields $ hashToId m
  liftIO $ putStrLn $ "PUT node"
  response :: GRpcReply Hash
    <- liftIO $ gRpcCall @'MsgProtoBuf @DagStore @"DagStore" @"PutNode" client n
  case response of
    GRpcOk ph -> liftEither $ fromProtoHash $ Const ph
    x -> throwError $ "PUT: error response was: " ++ show x


