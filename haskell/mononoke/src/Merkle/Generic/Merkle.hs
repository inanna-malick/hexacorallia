{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Merkle.Generic.Merkle
  ( Lazy (..),
    Local (..),
    PartialUpdate (OldStructure, NewStructure),
    module Merkle.Generic.Merkle,
  )
where

--------------------------------------------

--------------------------------------------
import Merkle.Generic.BlakeHash
import Merkle.Generic.CanonicalForm (CanonicalForm (..))
import Merkle.Generic.HRecursionSchemes
import Merkle.Generic.Merkle.Inner
import Optics

--------------------------------------------

fetchLazyT ::
  forall f m.
  Monad m =>
  NatM
    m
    (Term (Lazy m f))
    (Local f (Term (Lazy m f)))
fetchLazyT (Term lazy) = fetchLazy lazy

fetchLazy ::
  forall f m x.
  Monad m =>
  NatM
    m
    (Lazy m f x)
    (Local f x)
fetchLazy lazy = do
  inner <- lazy ^. #node
  pure $ Local (lazy ^. #hash) inner

partialUpdateHash :: (Term (PartialUpdate m f)) :-> Hash
partialUpdateHash x = x ^. #hash

partialUpdateLayer ::
  (Monad m, HFunctor f) =>
  NatM m (Term (PartialUpdate m f)) (f (Term (PartialUpdate m f)))
partialUpdateLayer (Term (NewStructure (Local _h f))) = pure f
partialUpdateLayer (Term (OldStructure (Term ((Lazy _h f))))) = hfmap (Term . OldStructure) <$> f

lazyExpandHash ::
  forall m f.
  HFunctor f =>
  Monad m =>
  NatM m Hash (f Hash) ->
  Hash :-> (Term (Lazy m f))
lazyExpandHash fetch = ana f
  where
    f :: Coalg (Lazy m f) Hash
    f h = Lazy h (fetch h)

-- upload some partially updated structure, returning (for convenience) a lazy representation of same
-- TODO: consider just returning a lazy expansion of the hash instead of storing in memory
commitPartialUpdate ::
  forall m f.
  ( Monad m,
    HTraversable f,
    HFunctor f
  ) =>
  NatM m (f Hash) Hash ->
  NatM m (Term (PartialUpdate m f)) (Term ((Lazy m f)))
commitPartialUpdate upload = hcataM f
  where
    f :: AlgM m (PartialUpdate m f) (Term (Lazy m f))
    f (OldStructure old) = pure old -- already uploaded lazy structure
    f (NewStructure (Local _h l)) = do
      -- TODO: compare local and remote hashes
      h <- upload $ hfmap (view #hash . unTerm) l
      pure $ Term $ Lazy h $ pure l

oldStructure ::
  Term (Lazy m f) :-> Term (PartialUpdate m f)
oldStructure = Term . OldStructure

newStructureT ::
  forall m f.
  ( CanonicalForm f,
    HTraversable f,
    HFunctor f
  ) =>
  Term f :-> Term (PartialUpdate m f)
newStructureT = hcata f
  where
    f x = Term $ NewStructure $ Local (canonicalHash $ hfmap partialUpdateHash x) x

newStructure ::
  forall m f.
  ( CanonicalForm f,
    HTraversable f,
    HFunctor f
  ) =>
  f (Term (PartialUpdate m f)) :-> Term (PartialUpdate m f)
newStructure x = Term . NewStructure $ Local h x
  where
    h = canonicalHash $ hfmap partialUpdateHash x

liftToLocalMerkle ::
  forall f.
  ( CanonicalForm f,
    HTraversable f,
    HFunctor f
  ) =>
  Term f :-> Term (Local f)
liftToLocalMerkle = hcata f
  where
    f n = Term $ Local (canonicalHash $ hfmap (view #hash . unTerm) n) n

-- -- what if 'm' is Identity for this?
liftLocalToLazy :: (Applicative m, HFunctor f) => Term (Local f) :-> Term (Lazy m f)
liftLocalToLazy = hcata f
  where
    f l = Term $ Lazy (l ^. #hash) (pure $ l ^. #node)
