{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Merkle.Generic.Store
  ( Store (..),
    fetchNode,
    uploadNode,
    transformStore,
  )
where

--------------------------------------------

import Data.Singletons.TH (SingI)
import Merkle.Generic.BlakeHash
import Merkle.Generic.HRecursionSchemes

--------------------------------------------

fetchNode :: Store m f -> NatM m Hash (f Hash)
fetchNode = sRead

uploadNode :: Store m f -> NatM m (f Hash) Hash
uploadNode = sWrite

-- TODO: expose partial tree upload here

-- | a hash-addressed store via which merkle tree layers of
--   type f can be fetched via Monad m
data Store m f = Store
  { sRead :: NatM m Hash (f Hash),
    sWrite :: NatM m (f Hash) Hash
  }

transformStore ::
  forall m1 m2 f.
  (forall x. m1 x -> m2 x) ->
  Store m1 f ->
  Store m2 f
transformStore f (Store read1 write1) = Store read2 write2
  where
    read2 :: NatM m2 Hash (f Hash)
    read2 h = f $ read1 h
    write2 :: NatM m2 (f Hash) Hash
    write2 x = f $ write1 x
