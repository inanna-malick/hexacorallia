{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Merkle.Generic.DAGStore.Types where

-------------------------------------------
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Const (Const (..))
import Data.Kind (Type)
import GHC.Generics
-------------------------------------------
import qualified Merkle.Generic.BlakeHash as BH
import Merkle.Generic.CanonicalForm (CanonicalForm)
import qualified Merkle.Generic.CanonicalForm as Canonical
import Merkle.Generic.HRecursionSchemes
import Mu.Quasi.GRpc
import Mu.Schema hiding (Term (..))

-------------------------------------------

grpc "GRPCStore" id "../../proto/dagstore.proto"

instance ToSchema GRPCStore "Id" Canonical.Id

instance FromSchema GRPCStore "Id" Canonical.Id

instance ToSchema GRPCStore "Hash" Canonical.Hash

instance FromSchema GRPCStore "Hash" Canonical.Hash

data Header = Header
  { header_id :: Maybe Canonical.Id,
    header_hash :: Maybe Canonical.Hash
  }
  deriving (Eq, Ord, Show, Generic)

instance ToSchema GRPCStore "Header" Header

instance FromSchema GRPCStore "Header" Header

data Node = Node
  { node_data :: B.ByteString,
    node_links :: [Header]
  }
  deriving (Eq, Ord, Show, Generic)

instance ToSchema GRPCStore "Node" Node

instance FromSchema GRPCStore "Node" Node

toCanonicalNode :: Node -> Maybe Canonical.Node
toCanonicalNode (Node nd nl) = Canonical.Node nd <$> traverse toCanonicalHeader nl

toCanonicalHeader :: Header -> Maybe Canonical.Header
toCanonicalHeader (Header (Just hid) (Just hh)) = Just $ Canonical.Header hid hh
toCanonicalHeader (Header _ _) = Nothing

fromCanonicalNode :: Canonical.Node -> Node
fromCanonicalNode (Canonical.Node nd nl) = Node nd $ fmap fromCanonicalHeader nl

fromCanonicalHeader :: Canonical.Header -> Header
fromCanonicalHeader (Canonical.Header hid hh) = Header (Just hid) (Just hh)

data NodeWithHeaderP = NodeWithHeaderP
  { header :: Maybe Header,
    node :: Maybe Node
  }
  deriving (Eq, Ord, Show, Generic)

instance ToSchema GRPCStore "NodeWithHeader" NodeWithHeaderP

instance FromSchema GRPCStore "NodeWithHeader" NodeWithHeaderP

data GetRespP = GetRespP
  { requested_node :: Maybe Node,
    extra_nodes :: [NodeWithHeaderP]
  }
  deriving (Eq, Ord, Show, Generic)

instance ToSchema GRPCStore "GetResp" GetRespP

instance FromSchema GRPCStore "GetResp" GetRespP

fromProtoHash' :: Canonical.Hash -> String `Either` BH.RawBlakeHash
fromProtoHash' = maybe (Left "invalid hash") (Right) . BH.bytesToHash . Canonical.hash_data

fromProtoHash :: NatM (Either String) (Const Canonical.Hash) BH.Hash
fromProtoHash = fmap Const . fromProtoHash' . getConst

-- -- parse a single node
fromProto ::
  forall k (f :: (k -> Type) -> k -> Type).
  ( HTraversable f,
    CanonicalForm f
  ) =>
  NatM (Either String) (Const Canonical.Node) (f BH.Hash)
fromProto (Const Canonical.Node {node_data, node_links}) = do
  let f :: Canonical.Header -> String `Either` (Canonical.Id, BH.RawBlakeHash)
      f Canonical.Header {header_id, header_hash} = do
        let header_id' = header_id
        header_hash' <- fromProtoHash' header_hash
        pure (header_id', header_hash')

  mappings <- traverse f node_links
  m <- Canonical.fromCanonicalForm (Const $ BL.fromStrict node_data)
  Canonical.idToHash mappings m

type PartialTree f = f (Context (Tagged BH.Hash `HCompose` f) BH.Hash)

-- recursively parse nodes building up a tree of them, TODO: a type for that, term of HEither Hash M
fromProtoGetResp ::
  forall f.
  ( HTraversable f,
    CanonicalForm f
  ) =>
  NatM (Either String) (Const GetRespP) (PartialTree f)
fromProtoGetResp (Const gr) = do
  let unpackNode ::
        Maybe Node ->
        String `Either` Canonical.Node
      unpackNode mn = do
        pn <- maybe (Left "requested node not present") Right $ mn
        n <- maybe (Left "node fields not present") Right $ toCanonicalNode pn
        pure n

      unpackNodeWithHeader ::
        NodeWithHeaderP ->
        String `Either` (BH.RawBlakeHash, Canonical.Node)
      unpackNodeWithHeader mnh = do
        n <- unpackNode $ node mnh
        mh <- maybe (Left "header not present") Right $ header mnh
        ph <- maybe (Left "header fields not present") Right $ toCanonicalHeader mh
        h <- fromProtoHash' $ Canonical.header_hash ph
        pure (h, n)

  node <- (unpackNode $ requested_node gr) >>= fromProto . Const

  -- at this point we DO NOT HAVE TYPE INFO
  -- so CANNOT decode nodes yet, just raw blake hashes and unparsed nodes
  nodes <- traverse unpackNodeWithHeader $ extra_nodes gr

  let deref :: CoalgPartialM (Either String) (Tagged BH.Hash `HCompose` f) BH.Hash
      deref h = do
        case lookup (getConst h) nodes of
          Nothing -> pure $ R h
          Just pn -> do
            nn <- fromProto $ Const pn
            pure . L . HC $ Tagged h nn

  htraverse (anaPartialM deref) node
