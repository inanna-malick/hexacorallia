{-# LANGUAGE RecordWildCards #-}

module Merkle.GUI.Messages
  ( module Merkle.GUI.Messages
  , SpawnPopup(..)
  ) where

--------------------------------------------
import           Merkle.GUI.Core
import           Merkle.GUI.Modal
--------------------------------------------


data BrowserNavigation m
  = Focus (FocusWIPT m)


data UpdateBranchState
  = ChangeFocus BranchFocus -- blocked if IPC is Just (NOTE: what does this mean? zero context lol)


