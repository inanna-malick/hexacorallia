{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Merkle.Generic.Merkle.Inner (Lazy (Lazy), Local (Local), PartialUpdate (OldStructure, NewStructure)) where

--------------------------------------------

import Merkle.Generic.BlakeHash
import Merkle.Generic.HRecursionSchemes
--------------------------------------------
import Optics

-- | Remotely persisted node in a merkle Tree
data Lazy m f g i = Lazy
  { _hash :: Hash i,
    _node :: m (f g i)
  }

instance (Functor m, HFunctor f) => HFunctor (Lazy m f) where
  hfmap f (Lazy h m) = Lazy h (hfmap f <$> m)

-- | Locally constructed node in a merkle Tree
data Local f g i = Local
  { _hash :: Hash i,
    _node :: f g i
  }

instance HFunctor f => HFunctor (Local f) where
  hfmap f (Local h n) = Local h (hfmap f n)

-- | Single layer of a locally constructed merkle tree with remotely persisted subtrees (or root node)
data PartialUpdate m f g i
  = NewStructure {_new :: Local f g i}
  | OldStructure {_old :: Term (Lazy m f) i}

instance (Functor m, HFunctor f) => HFunctor (PartialUpdate m f) where
  hfmap f (NewStructure l) = NewStructure $ hfmap f l
  hfmap _ (OldStructure l) = OldStructure l

instance (Monad m, HTraversable f) => HTraversable (PartialUpdate m f) where
  hmapM nat (NewStructure (Local h l)) = NewStructure . Local h <$> hmapM nat l
  hmapM _ (OldStructure l) = pure $ OldStructure l

  htraverse nat (NewStructure (Local h l)) = NewStructure . Local h <$> htraverse nat l
  htraverse _ (OldStructure l) = pure $ OldStructure l

makeFieldLabelsFor [("_hash", "hash"), ("_node", "node")] ''Local
makeFieldLabelsFor [("_hash", "hash"), ("_node", "node")] ''Lazy
makeFieldLabelsFor [("_new", "new"), ("_old", "old")] ''PartialUpdate

-- human-written instances to simplify looking into a Term (Lazy/Local)
instance (k ~ A_Lens, a ~ Hash i, b ~ Hash i) => LabelOptic "hash" k (Term (Local f) i) (Term (Local f) i) a b where
  labelOptic = lensVL $ \f (Term (Local h l)) -> Term . flip Local l <$> f h

instance (k ~ A_Lens, a ~ Hash i, b ~ Hash i) => LabelOptic "hash" k (Term (Lazy m f) i) (Term (Lazy m f) i) a b where
  labelOptic = lensVL $ \f (Term (Lazy h l)) -> Term . flip Lazy l <$> f h

instance (k ~ A_Lens, a ~ Hash i, b ~ Hash i) => LabelOptic "hash" k (Term (PartialUpdate m f) i) (Term (PartialUpdate m f) i) a b where
  labelOptic = lensVL g
    where
      g f (Term (NewStructure (Local h l))) = Term . NewStructure . flip Local l <$> f h
      g f (Term (OldStructure (Term (Lazy h l)))) = Term . OldStructure . Term . flip Lazy l <$> f h
      g _ (Hole _) = error "hole type is uninhabited"

-- human-written instances to simplify looking into a Term (Lazy/Local)
instance (k ~ A_Lens, a ~ f (Term (Local f)), b ~ f (Term (Local f))) => LabelOptic "node" k (Term (Local f) i) (Term (Local f) i) (a i) (b i) where
  labelOptic = lensVL $ \f (Term (Local h l)) -> Term . Local h <$> f l

instance (k ~ A_Lens, a ~ m (f (Term (Lazy m f)) i), b ~ m (f (Term (Lazy m f)) i)) => LabelOptic "node" k (Term (Lazy m f) i) (Term (Lazy m f) i) a b where
  labelOptic = lensVL $ \f (Term (Lazy h ml)) -> Term . Lazy h <$> f ml
