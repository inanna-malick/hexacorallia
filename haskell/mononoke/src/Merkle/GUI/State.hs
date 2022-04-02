{-# LANGUAGE RecordWildCards #-}

module Merkle.GUI.State where


--------------------------------------------
import qualified Data.Map as Map
--------------------------------------------
import           Merkle.App.LocalState
import           Merkle.Bonsai.Types
import           Merkle.GUI.Core
--------------------------------------------

-- TODO: expose error possibility instead of throwing error inline, partial function
fromLocalState :: Monad m => Store m -> LocalState -> BranchState m
fromLocalState store ls
  = BranchState
   -- FIXME: standardize repr, until then treat localstate current branch as main branch
  { bsMainBranch = expandHash (sRead store) $ either error id $ lsCurrentCommit ls
  , bsBranches = fmap (expandHash (sRead store)) <$> Map.toList (branches ls)
  , bsFocus = MainBranch
  }


data BranchState m
  = BranchState
  { bsMainBranch :: LMMT m 'CommitT
  , bsBranches :: [(String, LMMT m 'CommitT)]
  , bsFocus :: BranchFocus
  }


-- TODO: partial function, if branchstate is invalid
bsFocusedCommit :: BranchState m -> LMMT m 'CommitT
bsFocusedCommit (BranchState main _ MainBranch) = main
bsFocusedCommit (BranchState _ branchList (OtherBranch branch)) = maybe undefined id $ lookup branch branchList



instance Show (BranchState m) where
  show BranchState{..} = mconcat
    [ "BranchState { main: \n"
    , show $ hashOfLMMT bsMainBranch
    , "\n, branches: \n"
    , show $ fmap (fmap hashOfLMMT) bsBranches
    , "\n, focus: \n"
    , show bsFocus
    ]
