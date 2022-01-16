{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Merkle.Generic.Store
  ( Store(..)
  , fetchNode
  , uploadNode
  )
  where

--------------------------------------------
import           Merkle.Generic.HRecursionSchemes
import           Merkle.Generic.BlakeHash
--------------------------------------------

fetchNode :: Store m f -> NatM m Hash (f Hash)
fetchNode = sRead

uploadNode :: Store m f -> NatM m (f Hash) Hash
uploadNode = sWrite


-- TODO: expose partial tree upload here

-- | a hash-addressed store via which merkle tree layers of
--   type f can be fetched via Monad m
data Store m f
  = Store
  { sRead  :: NatM m Hash (f Hash)
  , sWrite :: NatM m (f Hash) Hash
  }
