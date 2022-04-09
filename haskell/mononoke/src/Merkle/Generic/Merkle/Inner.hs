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
  { hash :: Hash i,
    node :: m (f g i)
  }

instance (Functor m, HFunctor f) => HFunctor (Lazy m f) where
  hfmap f (Lazy h m) = Lazy h (hfmap f <$> m)

-- | Locally constructed node in a merkle Tree
data Local f g i = Local
  { hash :: Hash i,
    node :: f g i
  }

instance HFunctor f => HFunctor (Local f) where
  hfmap f (Local h n) = Local h (hfmap f n)

-- | Single layer of a locally constructed merkle tree with remotely persisted subtrees (or root node)
data PartialUpdate m f g i
  = NewStructure {new :: Local f g i}
  | OldStructure {old :: Term (Lazy m f) i}

instance (Functor m, HFunctor f) => HFunctor (PartialUpdate m f) where
  hfmap f (NewStructure l) = NewStructure $ hfmap f l
  hfmap _ (OldStructure l) = OldStructure l

instance (Monad m, HTraversable f) => HTraversable (PartialUpdate m f) where
  hmapM nat (NewStructure (Local h l)) = NewStructure . Local h <$> hmapM nat l
  hmapM _ (OldStructure l) = pure $ OldStructure l

  htraverse nat (NewStructure (Local h l)) = NewStructure . Local h <$> htraverse nat l
  htraverse _ (OldStructure l) = pure $ OldStructure l

makeFieldLabelsFor [("hash", "hash"), ("node", "node")] ''Local
makeFieldLabelsFor [("hash", "hash"), ("node", "node")] ''Lazy
makeFieldLabelsFor [("new", "new"), ("old", "old")] ''PartialUpdate

-- lnode :: Local f g i -> f g i
-- lnode l = l ^. #node
