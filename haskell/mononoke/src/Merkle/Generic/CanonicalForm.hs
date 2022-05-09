{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE RankNTypes #-}

-- | Canonical form of nodes in some hashed generic merkle DAG
--   Agnostic with regard to backend
module Merkle.Generic.CanonicalForm where

import qualified Control.Monad.State as S
import qualified Data.Aeson
import qualified Data.ByteString as B
import Data.ByteString.Builder as BB (toLazyByteString, word32LE)
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Const (Const (..))
import Data.Kind (Type)
import Data.Singletons.TH
import Data.Word
import GHC.Generics
import qualified Merkle.Generic.BlakeHash as BH
import Merkle.Generic.HRecursionSchemes

-- | some canonical encoding (such that it can be stored in a hash store)
--   with decode function
class CanonicalForm f where
  toCanonicalForm :: f (Const Id) :=> BL.ByteString
  fromCanonicalForm :: NatM (Either String) (Const BL.ByteString) (f (Const Id))

  canonicalHash :: HTraversable f => f BH.Hash :-> BH.Hash
  canonicalHash = Const . nodeToCanonicalHash . hashToId

data Id = Id {id_data :: Word32}
  deriving (Eq, Ord, Show, Generic)

instance Data.Aeson.ToJSON Id where
  toJSON = Data.Aeson.toJSON . id_data
  toEncoding = Data.Aeson.toEncoding . id_data

instance Data.Aeson.FromJSON Id where
  parseJSON = fmap Id . Data.Aeson.parseJSON

data Hash = Hash {hash_data :: B.ByteString}
  deriving (Eq, Ord, Show, Generic)

data Header = Header
  { header_id :: Id,
    header_hash :: Hash
  }
  deriving (Eq, Ord, Show, Generic)

data Node = Node
  { node_data :: B.ByteString,
    node_links :: [Header]
  }
  deriving (Eq, Ord, Show, Generic)

idToHash ::
  forall k (f :: (k -> Type) -> k -> Type).
  HTraversable f =>
  [(Id, BH.RawBlakeHash)] ->
  NatM (Either String) (f (Const Id)) (f BH.Hash)
idToHash headers m = htraverse f m
  where
    f :: NatM (Either String) (Const Id) BH.Hash
    f (Const idp) =
      Const
        <$> (maybe (Left $ "ID lookup failure for " ++ show idp) Right $ lookup idp headers)

hashToId' ::
  forall k f (i :: k) x.
  SingI i =>
  HTraversable f =>
  f (Const x) i ->
  (f (Const Id) i, [(x, Id)])
hashToId' m = flip S.runState [] $ hmapM f m
  where
    f ::
      NatM
        (S.State [(x, Id)])
        (Const x)
        (Const Id)
    f (Const rawHash) = do
      mappings <- S.get
      let nextId = Id $ fromInteger $ toInteger $ length mappings
          mapping = (rawHash, nextId)
          mappings' = mappings ++ [mapping]
      S.put mappings'
      pure $ Const nextId

hashToId ::
  ( HTraversable f,
    CanonicalForm f
  ) =>
  f BH.Hash :=> Node
hashToId m =
  Node
    { node_data = BL.toStrict $ toCanonicalForm m',
      node_links = headers
    }
  where
    (m', mappings) = hashToId' m
    headers = fmap f mappings
    f (rh, idp) =
      Header
        { header_id = idp,
          header_hash = Hash $ BH.unpackHash' rh
        }

-- | Blake2s
-- exact copy of this from the rust side:
--
-- use blake2::Digest;
-- let mut hasher = blake2::Blake2s::new();
-- for link in self.links.iter() {
--     hasher.update(&link.id.0.to_be_bytes());
--     hasher.update(link.hash.0.as_slice());
-- }
-- hasher.update(&self.data.0);
-- let hash = hasher.finalize();
-- Hash(hash)
nodeToCanonicalHash :: Node -> BH.RawBlakeHash
nodeToCanonicalHash n = BH.doHash' $ links ++ [node_data n]
  where
    links = foldMap f (node_links n)
    f h =
      [ BL.toStrict $ BB.toLazyByteString $ word32LE $ id_data $ header_id h,
        hash_data $ header_hash h
      ]
