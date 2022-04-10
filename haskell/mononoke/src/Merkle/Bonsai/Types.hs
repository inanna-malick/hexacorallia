{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Merkle.Bonsai.Types
  ( module Merkle.Bonsai.Types,
    Hash,
    Generic.sRead,
    Generic.sWrite,
    -- components of singleton type tag used for Bonsai GADT
    MTag (..),
    SMTag (..),
  )
where

--------------------------------------------
import Control.Monad.Except
import Data.Aeson as AE
import Data.Aeson.Types as AE
import qualified Data.ByteString.Lazy as LB
import Data.Functor.Const (Const (..))
import Data.List.NonEmpty (NonEmpty (..), intersperse)
import Data.Map.Strict (Map)
import Data.Singletons.TH
import qualified Data.Text as T
import GHC.Generics
--------------------------------------------
import Merkle.Bonsai.Types.Tags
import Merkle.Generic.BlakeHash
import qualified Merkle.Generic.DAGStore as DAG
import Merkle.Generic.HRecursionSchemes as HR
import qualified Merkle.Generic.Merkle as M
import qualified Merkle.Generic.Store as Generic

--------------------------------------------

data M a i where
  -- snapshots:
  Snapshot ::
    a 'FileTree -> -- snapshot of file tree in commit
    a 'CommitT -> -- originating commit
    [a 'SnapshotT] -> -- parent snapshots, if any
    M a 'SnapshotT
  -- file tree entries:
  File ::
    SnapshotFile a ->
    M a 'FileTree
  Dir ::
    --  TODO: will need canonical on-disk map repr/cannonical hash
    Map Path (a 'FileTree) -> -- children
    M a 'FileTree
  -- commits:
  NullCommit ::
    M a 'CommitT
  Commit ::
    String -> -- commit message
    [Change a] -> -- list of inline changes
    NonEmpty (a 'CommitT) -> -- parent commits
    M a 'CommitT
  -- blobs:
  Blob ::
    String -> -- TODO: use bytestring, currently string to simplify experimentations
    M a 'BlobT

-- repo state:
-- NOTE: this should generally be written to the disk instead of a remote store,
--       to avoid orphan merkle DAG nodes
--       NOTE: this really doesn't fit here, should be its own thing. however, it is a core type

instance ToJSON x => ToJSON (M (Const x) i) where
  toJSON (Snapshot ft oc ps) =
    object
      [ "file_tree" .= ft,
        "orig_commit" .= oc,
        "parent_snapshots" .= ps
      ]
  toJSON (File sf) =
    object
      [ "tag" .= ("file" :: T.Text),
        "contents" .= sf
      ]
  toJSON (Dir fs) =
    object
      [ "tag" .= ("dir" :: T.Text),
        "contents" .= fs
      ]
  toJSON NullCommit = AE.Null
  toJSON (Commit m cs ps) =
    object
      [ "msg" .= m,
        "changes" .= cs,
        "parent_commits" .= ps
      ]
  toJSON (Blob s) = AE.String $ T.pack s

-- TODO: round trip testing! apparent failure in this, "type" vs "tag" as tag type
instance FromJSON x => FromJSON (M (Const x) 'FileTree) where
  parseJSON (AE.Object v) = do
    typ :: T.Text <- v .: "tag"
    case typ of
      "dir" -> Dir <$> v .: "contents"
      "file" -> File <$> v .: "contents"
      t -> fail $ "unexpected FileTree type: " ++ T.unpack t
  parseJSON invalid =
    prependFailure
      "parsing FileTree failed, "
      (typeMismatch "Object" invalid)

instance FromJSON x => FromJSON (M (Const x) 'SnapshotT) where
  parseJSON (AE.Object v) =
    Snapshot
      <$> v .: "file_tree"
      <*> v .: "orig_commit"
      <*> v .: "parent_snapshots"
  parseJSON invalid =
    prependFailure
      "parsing Snapshot failed, "
      (typeMismatch "Object" invalid)

instance FromJSON x => FromJSON (M (Const x) 'CommitT) where
  parseJSON (AE.Null) = pure NullCommit
  parseJSON (AE.Object v) =
    Commit
      <$> v .: "msg"
      <*> v .: "changes"
      <*> v .: "parent_commits"
  parseJSON invalid =
    prependFailure
      "parsing blob failed, "
      (typeMismatch "Null or Object" invalid)

instance FromJSON x => FromJSON (M (Const x) 'BlobT) where
  parseJSON (AE.String v) = pure $ Blob $ T.unpack v
  parseJSON invalid =
    AE.prependFailure
      "parsing blob failed, "
      (AE.typeMismatch "String" invalid)

showHash :: forall (i :: MTag). SingI i => Hash i -> String
showHash h =
  let h' = take 6 $ T.unpack $ hashToText $ getConst h
   in "[" ++ typeTagName (sing :: Sing i) ++ ":" ++ h' ++ "]"

instance HFunctor M where
  hfmap f (Snapshot tree orig parents) = Snapshot (f tree) (f orig) (fmap f parents)
  hfmap f (File (SnapshotFile blob lastMod prev)) = File $ SnapshotFile (f blob) (f lastMod) (fmap f prev)
  hfmap f (Dir children) = Dir (fmap f children)
  hfmap _ NullCommit = NullCommit
  hfmap f (Commit msg changes parents) =
    let f' Change {..} = case _change of
          Add blob -> Change {_path = _path, _change = Add $ f blob}
          Del -> Change {_path = _path, _change = Del}
     in Commit msg (fmap f' changes) (fmap f parents)
  hfmap _ (Blob x) = Blob x

instance HTraversable M where
  hmapM f (Snapshot tree orig parents) = do
    tree' <- f tree
    orig' <- f orig
    parents' <- traverse f parents
    pure $ Snapshot tree' orig' parents'
  hmapM f (File (SnapshotFile blob lastMod prev)) = do
    blob' <- f blob
    lastMod' <- f lastMod
    prev' <- traverse f prev
    pure $ File $ SnapshotFile blob' lastMod' prev'
  hmapM f (Dir children) = do
    children' <- traverse f children
    pure $ Dir children'
  hmapM _ NullCommit = pure NullCommit
  hmapM f (Commit msg changes parents) =
    let f' Change {..} = case _change of
          Add blob -> do
            blob' <- f blob
            pure $ Change {_path = _path, _change = Add blob'}
          Del -> pure $ Change {_path = _path, _change = Del}
     in do
          changes' <- traverse f' changes
          parents' <- traverse f parents
          pure $ Commit msg changes' parents'
  hmapM _ (Blob x) = pure $ Blob x

  htraverse f (Snapshot tree orig parents) =
    Snapshot <$> f tree <*> f orig <*> traverse f parents
  htraverse f (File (SnapshotFile blob lastMod prev)) =
    File <$> (SnapshotFile <$> f blob <*> f lastMod <*> traverse f prev)
  htraverse f (Dir children) =
    Dir <$> traverse f children
  htraverse _ NullCommit = pure NullCommit
  htraverse f (Commit msg changes parents) =
    let f' Change {..} = case _change of
          Add blob -> Change _path <$> (fmap Add . f) blob
          Del -> pure $ Change {_path = _path, _change = Del}
     in Commit msg <$> traverse f' changes <*> traverse f parents
  htraverse _ (Blob x) = pure $ Blob x

-- type StoreRead  m = NatM (MaybeT m) Hash (M Hash)
type StoreRead m = NatM m Hash (M Hash)

type StoreWrite m = NatM m (M Hash) Hash

type Store m = Generic.Store m M

instance DAG.CanonicalForm M where
  toCanonicalForm = AE.encode
  fromCanonicalForm ::
    forall (i :: MTag).
    SingI i =>
    Const LB.ByteString i ->
    String `Either` (M (Const DAG.Id) i)
  fromCanonicalForm (Const b) = case sing @i of
    SSnapshotT -> AE.eitherDecode b
    SFileTree -> AE.eitherDecode b
    SCommitT -> AE.eitherDecode b
    SBlobT -> AE.eitherDecode b

mkDagStore ::
  forall m.
  ( MonadError String m,
    MonadIO m
  ) =>
  DAG.GrpcClient ->
  Store m
mkDagStore = DAG.mkDagStore

-- type aliases using the new, more elegant repr - will gradually phase it in
type Lazy m = M.Lazy m M

type Local = M.Local M

type PartialUpdate m = M.PartialUpdate m M

type Path = String -- TODO use Text

concatPath :: NonEmpty Path -> String
concatPath = foldMap id . intersperse "/"

add :: NonEmpty Path -> a 'BlobT -> Change a
add p a = Change {_path = p, _change = Add a}

del :: NonEmpty Path -> Change a
del p = Change {_path = p, _change = Del}

data Change a = Change
  { _path :: NonEmpty Path,
    _change :: ChangeType a
  }
  deriving (Generic)

mapChange :: (a 'BlobT -> b 'BlobT) -> Change a -> Change b
mapChange f (Change p c) = Change p (mapChangeType f c)

mapChangeType :: (a 'BlobT -> b 'BlobT) -> ChangeType a -> ChangeType b
mapChangeType f (Add x) = Add (f x)
mapChangeType _ Del = Del

instance Show (Change (Term M)) where
  show (Change path Del) = "Del " ++ concatPath path
  show (Change path (Add (Term (Blob contents)))) =
    "Add " ++ concatPath path ++ ", contents:\n" ++ contents

instance Eq (Change (Term M)) where
  (Change path1 Del) == (Change path2 Del) = path1 == path2
  (Change path1 (Add (Term (Blob x1)))) == (Change path2 (Add (Term (Blob x2)))) = x1 == x2 && path1 == path2
  _ == _ = False

instance ToJSON (a 'BlobT) => ToJSON (Change a) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON (a 'BlobT) => FromJSON (Change a)

data ChangeType a
  = Add (a 'BlobT)
  | Del
  deriving (Generic)

instance ToJSON (a 'BlobT) => ToJSON (ChangeType a) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON (a 'BlobT) => FromJSON (ChangeType a)

data SnapshotFile a = SnapshotFile
  { sfBlob :: a 'BlobT,
    sfLastModifiedCommit :: a 'CommitT,
    sfPreviousIncarnations :: [a 'FileTree]
  }
  deriving (Generic)

instance (ToJSON (a 'BlobT), ToJSON (a 'CommitT), ToJSON (a 'FileTree)) => ToJSON (SnapshotFile a) where
  toEncoding = genericToEncoding defaultOptions

instance (FromJSON (a 'BlobT), FromJSON (a 'CommitT), FromJSON (a 'FileTree)) => FromJSON (SnapshotFile a)
