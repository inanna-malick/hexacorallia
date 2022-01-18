{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Merkle.GUI.WorkingMergeTrie (UpdateMergeTrie(..), browseMergeTrie) where

--------------------------------------------
import           Control.Concurrent.STM
import           Data.Functor.Compose
import           Data.Functor.Const
import           Data.Functor.Foldable (cata)
import           Data.Fix (Fix(..))
import           Data.List.NonEmpty (NonEmpty, nonEmpty)
import           Data.Singletons.TH (sing)
import qualified Data.Map as Map
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
--------------------------------------------
import           Merkle.Bonsai.MergeTrie
import qualified Merkle.Bonsai.Types.Render
import           Merkle.Bonsai.Types
import           Merkle.Bonsai.Types.Tags (typeTagName, typeTagName')
import           Merkle.GUI.BrowseWIPT
import           Merkle.GUI.Core
import           Merkle.GUI.Elements
import           Merkle.GUI.State
import           Merkle.Generic.HRecursionSchemes
--------------------------------------------


data UpdateMergeTrie m
  = ApplyChange (NonEmpty Path) (ChangeType (WIPT m))
  | RemoveChange (NonEmpty Path)
  | AddParent (LMMT m 'CommitT)
  | Reset
  | Finalize String -- finalize commit w/ message

instance Show (UpdateMergeTrie m) where
  show (ApplyChange p c) =
    let ctmapShim f (Add a) = Add (f a)
        ctmapShim _ Del = Del
        cmapShim f Change{..} = Change _path $ ctmapShim f _change
     in mconcat [ "ApplyChange: "
                , unlines $ Merkle.Bonsai.Types.Render.renderChange
                          $ cmapShim (Const . Merkle.Bonsai.Types.Render.renderWIPT)
                          $ Change p c
                ]
  show (RemoveChange p) = "RemoveChange: " ++ show p
  show (AddParent _) = "AddParent: todo"
  show Reset = "Reset"
  show (Finalize msg) = "Finalize, msg: " ++ msg


browseMergeTrie
  :: Handler (UpdateMergeTrie UI)
  -> Handler (FocusWIPT UI)
  -> TVar Minimizations
  -> InProgressCommit UI `Either` LMMT UI 'CommitT
  -> Fix (ErrorAnnotatedMergeTrie UI) `Either` Fix (MergeTrie UI)
  -> UI Element
browseMergeTrie modifyMergeTrieHandler focusHandler _minimizations ipcOrC eroot -- TODO: revisit 'ipc or c'
  = UI.div # withClass ["placeholder"]
