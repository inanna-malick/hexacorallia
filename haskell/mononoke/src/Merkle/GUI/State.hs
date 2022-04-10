{-# LANGUAGE RecordWildCards #-}

module Merkle.GUI.State where

--------------------------------------------
import qualified Data.Map as Map
--------------------------------------------
import Merkle.App.LocalState
import Merkle.Bonsai.Types
import Merkle.GUI.Core
import Merkle.Generic.HRecursionSchemes
import Merkle.Generic.Merkle hiding (Lazy)
--------------------------------------------
import Optics

-- TODO: expose error possibility instead of throwing error inline, partial function
fromLocalState :: Monad m => Store m -> LocalState -> BranchState m
fromLocalState store ls =
  BranchState
    { -- FIXME: standardize repr, until then treat localstate current branch as main branch
      bsMainBranch = lazyExpandHash (sRead store) $ either error id $ lsCurrentCommit ls,
      bsBranches = fmap (lazyExpandHash (sRead store)) <$> Map.toList (branches ls),
      bsFocus = MainBranch
    }

data BranchState m = BranchState
  { bsMainBranch :: Term (Lazy m) 'CommitT,
    bsBranches :: [(String, Term (Lazy m) 'CommitT)],
    bsFocus :: BranchFocus
  }

-- TODO: partial function, if branchstate is invalid
bsFocusedCommit :: BranchState m -> Term (Lazy m) 'CommitT
bsFocusedCommit (BranchState main _ MainBranch) = main
bsFocusedCommit (BranchState _ branchList (OtherBranch branch)) = maybe undefined id $ lookup branch branchList

instance Show (BranchState m) where
  show BranchState {..} =
    mconcat
      [ "BranchState { main: \n",
        show $ unTerm bsMainBranch ^. #hash,
        "\n, branches: \n",
        show $ fmap (fmap ((^. #hash) . unTerm)) bsBranches,
        "\n, focus: \n",
        show bsFocus
      ]
