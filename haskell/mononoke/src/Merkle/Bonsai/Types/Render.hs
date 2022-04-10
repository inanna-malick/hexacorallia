{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Merkle.Bonsai.Types.Render where

--------------------------------------------
import Data.Functor.Const (Const (..))
import Data.List (intersperse)
import Data.List.NonEmpty (toList)
import qualified Data.Map.Strict as Map
import Data.Singletons.TH
--------------------------------------------
import Merkle.Bonsai.Types
import Merkle.Generic.HRecursionSchemes as HR -- YOLO 420 SHINY AND CHROME
import Merkle.Render.Utils
import Optics ((^.))

--------------------------------------------

-- | algebra, assumes all sub-entities have been rendered down to a list of lines
renderM :: M (Const [String]) i -> Const [String] i
renderM (Repo branches snapshotIndex currentBranch) =
  let branches' = (\(k, v) -> [k ++ ": "] ++ getConst v) <$> Map.toList branches
      snapshotIndex' = (\(k, v) -> [show (getConst k)] ++ [": "] ++ getConst v) <$> Map.toList snapshotIndex
   in Const $
        mconcat [["Repo State:"]]
          ++ ( indent $
                 [ ["current branch: ", currentBranch],
                   ["branches:"] ++ indent branches',
                   ["snapshot index:"] ++ indent snapshotIndex'
                 ]
             )
renderM (Snapshot tree orig parents) =
  Const $
    mconcat [["Snapshot:"]]
      ++ ( indent $
             [ getConst tree,
               getConst orig,
               ["parents:"] ++ (indent $ (getConst <$> parents))
             ]
         )
renderM (File (SnapshotFile blob lastMod prev)) =
  Const $
    mconcat [["File:"]]
      ++ ( indent $
             [ getConst blob,
               getConst lastMod
             ]
               ++ fmap getConst prev
         )
renderM (Dir children) =
  let children' = (\(k, v) -> [k ++ ": "] ++ getConst v) <$> Map.toList children
   in Const $ mconcat [["Dir:"]] ++ indent children'
renderM NullCommit = Const ["NullCommit"]
renderM (Commit msg changes parents) =
  Const $
    mconcat [["Commit \"" ++ msg ++ "\""]]
      ++ ( indent $
             [ ["changes:"] ++ (indent $ (renderChange <$> changes)),
               ["parents:"] ++ (indent $ (getConst <$> toList parents))
             ]
         )
renderM (Blob x) = Const ["Blob: " ++ x]

renderChange :: Change (Const [String]) -> [String]
renderChange Change {..} = case _change of
  Del -> [renderPath _path ++ ": Del"]
  Add x -> [renderPath _path ++ ": Add: "] ++ getConst x
  where
    renderPath = mconcat . intersperse "/" . toList

-- TODO: can't hcatM LMMT b/c 'm' is in the stack that needs to be HTraversable
renderLMMT :: forall m (x :: MTag). SingI x => Monad m => Term (Lazy m) x -> m [String]
renderLMMT = getConst . hcata f
  where
    f :: Alg (Lazy m) (Const (m [String]))
    f lazy = Const $ do
      m' <- lazy ^. #node
      m'' <- hmapM (\x -> Const <$> getConst x) m'
      pure $ getConst $ renderM m''

showLMMT :: SingI x => Term (Lazy IO) x -> IO ()
showLMMT = (>>= const (pure ())) . (>>= traverse putStrLn) . renderLMMT
