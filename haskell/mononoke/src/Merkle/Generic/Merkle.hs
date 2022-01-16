{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Merkle.Generic.Merkle
  ( LazyMerkle(..), toLMT, fromLMT
  , LocalMerkle(..), PartialMerkleTreeLayer, PartialMerkleTree, NEPartialMerkleTree
  , wipToPartialMerkleTreeLayer, wipTreeToPartialMerkleTree
  , partialMerkleTreeToWIPT, partialMerkleTreeLayerToWIP
  )
  where

--------------------------------------------
import           Data.Functor.Compose
--------------------------------------------
import           Merkle.Generic.HRecursionSchemes
import           Merkle.Generic.BlakeHash
--------------------------------------------


type LazyMerkleTree m f = Term (LazyMerkle m f)

-- | Remotely persisted node in a merkle Tree
data LazyMerkle m f g i
  = LazyMerkle
  { lazyMerkleHash  :: Hash i
  , lazyMerkleFetch :: m (f g i)
  }

instance (Functor m, HFunctor f) => HFunctor (LazyMerkle m f) where
    hfmap f (LazyMerkle h m) = LazyMerkle h (hfmap f <$> m)




-- | Locally constructed node in a merkle Tree
data LocalMerkle m f g i
  = LocalMerkle
  { localMerkleHash :: Hash i
  , localMerkleNode :: f g i
  }

-- | Single layer of a locally constructed merkle tree with remotely persisted subtrees
type PartialMerkleTreeLayer m f = HEither (LazyMerkleTree m f) `HCompose` LocalMerkle m f

-- | locally constructed merkle tree with remotely persisted subtrees (or root node)
type PartialMerkleTree m f = Term (PartialMerkleTreeLayer m f)

-- | locally constructed merkle tree with remotely persisted subtrees and at
--   least one locally constructed node as the tree root
type NEPartialMerkleTree m f = LocalMerkle m f (Term (PartialMerkleTreeLayer m f))


instance (Functor m, HFunctor f) => HFunctor (LocalMerkle m f) where
    hfmap f (LocalMerkle h n) = LocalMerkle h (hfmap f n)


type LM m f = Tagged Hash `HCompose` Compose m `HCompose` f
type LMT m f = Term (LM m f)


fromLM :: LM m f x :-> LazyMerkle m f x
fromLM (HC (Tagged h (HC (Compose m)))) = LazyMerkle h m

fromLMT :: (Functor m, HFunctor f) => LMT m f :-> Term (LazyMerkle m f)
fromLMT = hcata (Term . fromLM)


toLM ::  LazyMerkle m f x :-> LM m f x
toLM (LazyMerkle h m) = HC $ Tagged h $ HC $ Compose m

toLMT :: (Functor m, HFunctor f) => Term (LazyMerkle m f) :-> LMT m f
toLMT = hcata (Term . toLM)

type WIP m f = HEither (LMT m f) `HCompose` Tagged Hash `HCompose` f
type WIPT m f = Term (WIP m f)

partialMerkleTreeToWIPT
  :: (Functor m, HFunctor f)
  => PartialMerkleTree m f :-> WIPT m f
partialMerkleTreeToWIPT = hcata (Term . partialMerkleTreeLayerToWIP)

partialMerkleTreeLayerToWIP
  :: (Functor m, HFunctor f)
  => PartialMerkleTreeLayer m f g :-> WIP m f g
partialMerkleTreeLayerToWIP (HC (L lazyMerkleTree)) = HC $ L $ toLMT lazyMerkleTree
partialMerkleTreeLayerToWIP (HC (R localLayer)) = localLayerToWIP localLayer

localLayerToWIP ::  LocalMerkle m f g :-> WIP m f g
localLayerToWIP (LocalMerkle h n) = HC $ R $ HC $ Tagged h n


wipToPartialMerkleTreeLayer
  :: (Functor m, HFunctor f)
  => WIP m f g :-> PartialMerkleTreeLayer m f g
wipToPartialMerkleTreeLayer (HC (L lmt)) = HC $ L $ fromLMT lmt
wipToPartialMerkleTreeLayer (HC (R (HC (Tagged h n)))) = HC $ R $ LocalMerkle h n


wipTreeToPartialMerkleTree
  :: (Functor m, HFunctor f)
  => WIPT m f :-> PartialMerkleTree m f
wipTreeToPartialMerkleTree = hcata (Term . wipToPartialMerkleTreeLayer)
