{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE OverloadedLabels #-}

module Merkle.Generic.Merkle
  ( Lazy(..)
  , Local(..), PartialUpdate(OldStructure, NewStructure)
  , toLMT, fromLMT
  , wipToPartialUpdate, wipTreeToPartialUpdateTree
  , partialUpdateToWIP, partialUpdateTreeToWIPT
  )
  where

--------------------------------------------
import           Data.Functor.Compose
--------------------------------------------
import           Merkle.Generic.HRecursionSchemes
import           Merkle.Generic.BlakeHash
import           Merkle.Generic.Merkle.Inner
import           Merkle.Generic.CanonicalForm (CanonicalForm(..))
--------------------------------------------
import Optics






fromLM :: LM m f x :-> Lazy m f x
fromLM (HC (Tagged h (HC (Compose m)))) = Lazy h m

fromLMT :: (Functor m, HFunctor f) => LMT m f :-> Term (Lazy m f)
fromLMT = hcata (Term . fromLM)


toLM ::  Lazy m f x :-> LM m f x
toLM l = HC $ Tagged (l ^. #hash) $ HC $ Compose (l ^. #node)

toLMT :: (Functor m, HFunctor f) => Term (Lazy m f) :-> LMT m f
toLMT = hcata (Term . toLM)


-- Lazy Merkle M
type LM m f = Tagged Hash `HCompose` Compose m `HCompose` f
type LMT m f = Term (LM m f)

type WIP m f = HEither (LMT m f) `HCompose` Tagged Hash `HCompose` f
type WIPT m f = Term (WIP m f)

partialUpdateTreeToWIPT
  :: (Functor m, HFunctor f)
  => Term (PartialUpdate m f) :-> WIPT m f
partialUpdateTreeToWIPT = hcata (Term . partialUpdateToWIP)

partialUpdateToWIP
  :: (Functor m, HFunctor f)
  => PartialUpdate m f g :-> WIP m f g
partialUpdateToWIP (OldStructure l) = HC $ L $ toLMT l
partialUpdateToWIP (NewStructure l) = localLayerToWIP l

localLayerToWIP ::  Local f g :-> WIP m f g
localLayerToWIP l = HC $ R $ HC $ Tagged (l ^. #hash) (l ^. #node)


wipToPartialUpdate
  :: (Functor m, HFunctor f)
  => WIP m f g :-> PartialUpdate m f g
wipToPartialUpdate (HC (L lmt)) = OldStructure $ fromLMT lmt
wipToPartialUpdate (HC (R (HC (Tagged h n)))) = NewStructure $ Local h n


wipTreeToPartialUpdateTree
  :: (Functor m, HFunctor f)
  => WIPT m f :-> Term (PartialUpdate m f)
wipTreeToPartialUpdateTree = hcata (Term . wipToPartialUpdate)


liftToLocalMerkle
  :: forall f
   . ( CanonicalForm f
     , HTraversable f
     , HFunctor f
     )
  => Term f :-> Term (Local f)
liftToLocalMerkle = hcata f
  where f n = Term $ Local (canonicalHash $ hfmap (view #hash . unTerm) n) n

-- -- what if 'm' is Identity for this?
liftLocalToLazy :: (Applicative m, HFunctor f) => Term (Local f) :-> Term (Lazy m f)
liftLocalToLazy = hcata f
  where f l = Term $ Lazy (l ^. #hash) (pure $ l ^. #node)
