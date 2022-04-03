{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}


module Merkle.Bonsai.Types
  ( module Merkle.Bonsai.Types
  , Hash
  , Generic.sRead, Generic.sWrite
  -- components of singleton type tag used for Bonsai GADT
  , MTag(..)
  , SMTag(..)
  ) where


import Data.Aeson as AE
import Data.Aeson.Types as AE
import GHC.Generics
--------------------------------------------
import           Control.Concurrent.STM
import           Control.Monad.Except
import           Data.Functor.Const (Const(..))
import qualified Data.ByteString.Lazy as LB
import           Data.Functor.Compose
import           Data.List.NonEmpty (NonEmpty(..), intersperse)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Singletons.TH
import qualified Data.Text as T
--------------------------------------------
import           Merkle.Bonsai.Types.Tags
import           Merkle.Generic.BlakeHash
import qualified Merkle.Generic.DAGStore as DAG
import qualified Merkle.Generic.Store as Generic
import           Merkle.Generic.HRecursionSchemes as HR
import qualified Merkle.Generic.Merkle as M
--------------------------------------------


type Path = String -- TODO use Text

concatPath :: NonEmpty Path -> String
concatPath = foldMap id . intersperse "/"


add :: NonEmpty Path -> a 'BlobT -> Change a
add p a = Change { _path = p, _change = Add a}

del :: NonEmpty Path -> Change a
del p = Change { _path = p, _change = Del}

data Change a
  = Change
  { _path::   NonEmpty Path
  , _change:: ChangeType a
  } deriving (Generic)

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

data SnapshotFile a
  = SnapshotFile
  { sfBlob                 ::  a 'BlobT
  , sfLastModifiedCommit   ::  a 'CommitT
  , sfPreviousIncarnations :: [a 'FileTree]
  } deriving (Generic)

instance (ToJSON (a 'BlobT), ToJSON (a 'CommitT), ToJSON (a 'FileTree)) => ToJSON (SnapshotFile a) where
    toEncoding = genericToEncoding defaultOptions
instance (FromJSON (a 'BlobT), FromJSON (a 'CommitT), FromJSON (a 'FileTree)) => FromJSON (SnapshotFile a)

data M a i where
  -- snapshots:
  Snapshot
    :: a 'FileTree    -- snapshot of file tree in commit
    -> a 'CommitT     -- originating commit
    -> [a 'SnapshotT] -- parent snapshots, if any
    -> M a 'SnapshotT

  -- file tree entries:
  File
    :: SnapshotFile a
    -> M a 'FileTree

  Dir
    --  TODO: will need canonical on-disk map repr/cannonical hash
    :: Map Path (a 'FileTree) -- children
    -> M a 'FileTree

  -- commits:
  NullCommit
    :: M a 'CommitT

  Commit
    :: String                -- commit message (TODO text)
    -> [Change a]            -- list of inline changes
    -> NonEmpty (a 'CommitT) -- parent commits
    -> M a 'CommitT

  -- blobs:
  Blob
    :: String -- TODO: use bytestring, currently string to simplify experimentations
    -> M a 'BlobT



instance ToJSON x => ToJSON (M (Const x) i) where
  toJSON (Snapshot ft oc ps) =
    object [ "file_tree" .= ft
           , "orig_commit" .= oc
           , "parent_snapshots" .= ps
           ]
  toJSON (File sf) =
    object [ "tag" .= ("file" :: T.Text)
           , "contents" .= sf
           ]
  toJSON (Dir fs) =
    object [ "tag" .= ("dir" :: T.Text)
           , "contents" .= fs
           ]
  toJSON NullCommit = AE.Null
  toJSON (Commit m cs ps) =
    object [ "msg" .= m
           , "changes" .= cs
           , "parent_commits" .= ps
           ]
  toJSON (Blob s) = AE.String $ T.pack s

-- TODO: round trip testing! apparent failure in this, "type" vs "tag" as tag type
instance FromJSON x => FromJSON (M (Const x) 'FileTree) where
    parseJSON (AE.Object v) = do
      typ :: T.Text <- v .: "tag"
      case typ of
        "dir"  -> Dir  <$> v .: "contents"
        "file" -> File <$> v .: "contents"
        t -> fail $ "unexpected FileTree type: " ++ T.unpack t
    parseJSON invalid    =
        prependFailure "parsing FileTree failed, "
            (typeMismatch "Object" invalid)

instance FromJSON x => FromJSON (M (Const x) 'SnapshotT) where
    parseJSON (AE.Object v) = Snapshot
        <$> v .: "file_tree"
        <*> v .: "orig_commit"
        <*> v .: "parent_snapshots"
    parseJSON invalid    =
        prependFailure "parsing Snapshot failed, "
            (typeMismatch "Object" invalid)

instance FromJSON x => FromJSON (M (Const x) 'CommitT) where
    parseJSON (AE.Null) = pure NullCommit
    parseJSON (AE.Object v) = Commit
        <$> v .: "msg"
        <*> v .: "changes"
        <*> v .: "parent_commits"
    parseJSON invalid    =
        prependFailure "parsing blob failed, "
            (typeMismatch "Null or Object" invalid)


instance FromJSON x => FromJSON (M (Const x) 'BlobT) where
    parseJSON (AE.String v) = pure $ Blob $ T.unpack v

    parseJSON invalid    =
        AE.prependFailure "parsing blob failed, "
            (AE.typeMismatch "String" invalid)



-- Lazy Merkle M (old repr)
type LMM m = Tagged Hash `HCompose` Compose m `HCompose` M
type LMMT m = Term (LMM m)


fetchLMMT :: Functor m => NatM m (LMMT m) ((Tagged Hash `HCompose` M) (LMMT m))
fetchLMMT (Term lmm) = fetchLMM lmm

fetchLMM :: Functor m => NatM m (LMM m x) ((Tagged Hash `HCompose` M) x)
fetchLMM (HC (Tagged h (HC (Compose m)))) = HC . Tagged h <$> m


flattenLMMT :: M (LMMT m) :-> M Hash
flattenLMMT = hfmap hashOfLMMT

hashOfLMMT :: LMMT m :-> Hash
hashOfLMMT (Term (HC (Tagged h _))) = h

type WIP m = HEither (LMMT m) `HCompose` Tagged Hash `HCompose` M
type WIPT m = Term (WIP m)

uploadWIPT
  :: forall m
   . Monad m
  => NatM m (M Hash) Hash
  -> NatM m (WIPT m) (LMMT m)
uploadWIPT _upload (Term (HC (L lmmt))) = pure lmmt
uploadWIPT upload (Term (HC (R (HC (Tagged _h m))))) = do
  lmmt <- hmapM (uploadWIPT upload) m
  h' <- upload $ hfmap hashOfLMMT lmmt
  -- TODO assert h == h'
  pure $ Term $ HC $ Tagged h' $ HC $ Compose $ pure lmmt

fetchWIPT :: Applicative m => NatM m (WIPT m) ((Tagged Hash `HCompose` M) (WIPT m))
fetchWIPT (Term (HC (L lmmt))) = hfmap unmodifiedWIP <$> fetchLMMT lmmt
fetchWIPT (Term (HC (R hct))) = pure hct

hashOfWIPT :: WIPT m :-> Hash
hashOfWIPT (Term (HC (L lmmt))) = hashOfLMMT lmmt
hashOfWIPT (Term (HC (R (HC (Tagged h _))))) = h

unmodifiedWIP :: LMMT m :-> WIPT m
unmodifiedWIP = Term . HC . L

modifiedWIP :: M (WIPT m) :-> WIPT m
modifiedWIP m = Term . HC . R . HC $ Tagged h m
  where
    h = DAG.canonicalHash $ hfmap hashOfWIPT m

modifiedWIP' :: Term M :-> WIPT m
modifiedWIP' m = hcata (Term . HC . R) $ hashMT m


showHash :: forall (i :: MTag). SingI i => Hash i -> String
showHash h =
  let h' = take 6 $ T.unpack $ hashToText $ getConst h
   in "[" ++ typeTagName (sing :: Sing i) ++ ":" ++ h' ++ "]"



-- | hash and lift (TODO: THIS IS NEEDED - dip into merkle lib for refs)
liftLMMT :: forall m. Applicative m => Term M :-> LMMT m
liftLMMT = hcata f
  where
    f x = Term $ HC $ Tagged{ _tag = h x, _elem = HC $ Compose $ pure x}
    h x = DAG.canonicalHash $ hfmap hashOfLMMT x


expandHash :: forall m. Monad m => StoreRead m -> Hash :-> (LMMT m)
expandHash get = ana f
  where
    f :: Coalg (LMM m) Hash
    f h = HC $ Tagged h $ HC $ Compose $ get h


uploadM :: Monad m => StoreWrite m -> NatM m (Term M) Hash
uploadM upload = hcataM upload





instance HFunctor M where
  hfmap f (Snapshot tree orig parents) = Snapshot (f tree) (f orig) (fmap f parents)
  hfmap f (File (SnapshotFile blob lastMod prev)) = File $ SnapshotFile (f blob) (f lastMod) (fmap f prev)
  hfmap f (Dir children) = Dir (fmap f children)
  hfmap _ NullCommit = NullCommit
  hfmap f (Commit msg changes parents) =
    let f' Change{..} = case _change of
          Add blob -> Change { _path = _path, _change = Add $ f blob}
          Del -> Change { _path = _path, _change = Del}
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
    let f' Change{..} = case _change of
          Add blob -> do
            blob' <- f blob
            pure $ Change { _path = _path, _change = Add blob'}
          Del -> pure $ Change { _path = _path, _change = Del}
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
    let f' Change{..} = case _change of
          Add blob -> Change _path <$> (fmap Add . f) blob
          Del -> pure $ Change { _path = _path, _change = Del}
     in Commit msg <$> traverse f' changes <*> traverse f parents
  htraverse _ (Blob x) = pure $ Blob x






-- TODO new module

data BlobStore
  = BlobStore
  { snapshotBS :: (Map (Hash 'SnapshotT) (M Hash 'SnapshotT))
  , fileTreeBS :: (Map (Hash 'FileTree)  (M Hash 'FileTree))
  , commitBS   :: (Map (Hash 'CommitT)   (M Hash 'CommitT))
  , blobBS     :: (Map (Hash 'BlobT)     (M Hash 'BlobT))
  }

emptyBlobStore :: BlobStore
emptyBlobStore
  = BlobStore
  { snapshotBS = Map.empty
  , fileTreeBS = Map.empty
  , commitBS   = Map.empty
  , blobBS     = Map.empty
  }

-- (sing :: Sing i)
getBlobStore :: forall (i :: MTag). Sing i -> Hash i -> BlobStore -> Maybe (M Hash i)
getBlobStore s h bs = case s of
  SSnapshotT -> Map.lookup h $ snapshotBS bs
  SFileTree  -> Map.lookup h $ fileTreeBS bs
  SCommitT   -> Map.lookup h $ commitBS   bs
  SBlobT     -> Map.lookup h $ blobBS     bs


putBlobStore :: forall (i :: MTag). Sing i -> Hash i -> M Hash i -> BlobStore -> BlobStore
putBlobStore s h m bs = case s of
  SSnapshotT -> bs { snapshotBS = Map.insert h m $ snapshotBS bs }
  SFileTree  -> bs { fileTreeBS = Map.insert h m $ fileTreeBS bs }
  SCommitT   -> bs { commitBS   = Map.insert h m $ commitBS   bs }
  SBlobT     -> bs { blobBS     = Map.insert h m $ blobBS     bs }


-- type StoreRead  m = NatM (MaybeT m) Hash (M Hash)
type StoreRead  m = NatM m Hash (M Hash)
type StoreWrite m = NatM m (M Hash) Hash

type Store m = Generic.Store m M


lazyLoadHash :: forall m. Monad m => Store m -> Hash :-> Term (M.Lazy m M)
lazyLoadHash store = ana f
  where
    f :: Hash :-> M.Lazy m M Hash
    f h = M.Lazy h (Generic.sRead store h)


stmIOStore :: MonadIO m => TVar BlobStore -> Store m
stmIOStore tvar
  = let store' = stmStore tvar
     in Generic.Store
  { Generic.sRead = \h   -> liftIO $ atomically $ Generic.sRead store' h
  , Generic.sWrite = \mh -> liftIO $ atomically $ Generic.sWrite store' mh
  }



hashMT :: Term M :-> Term (Tagged Hash `HCompose` M)
hashMT m = hcata f m
  where
    f :: M (Term (Tagged Hash `HCompose` M)) :-> Term (Tagged Hash `HCompose` M)
    f x = Term $ HC $ Tagged{ _tag = h x, _elem = x}
    h x = DAG.canonicalHash $ hfmap (_tag . getHC . unTerm) x


stmStore :: TVar BlobStore -> Store STM
stmStore tvar
  = Generic.Store
  { Generic.sRead = \h -> do
      bs <- readTVar tvar
      pure . maybe (error "hashmap lookup broken") id $ getBlobStore sing h bs
  , Generic.sWrite = \mh -> do
      let h = DAG.canonicalHash mh
      modifyTVar tvar $ \bs ->
        putBlobStore sing h mh bs
      pure h
  }


instance DAG.CanonicalForm M where
  toCanonicalForm = AE.encode
  fromCanonicalForm
    :: forall (i :: MTag)
    .  SingI i
    => Const LB.ByteString i
    -> String `Either` (M (Const DAG.Id) i)
  fromCanonicalForm (Const b) = case sing @i of
    SSnapshotT -> AE.eitherDecode b
    SFileTree  -> AE.eitherDecode b
    SCommitT   -> AE.eitherDecode b
    SBlobT     -> AE.eitherDecode b

mkDagStore
  :: forall m
   . ( MonadError String m
     , MonadIO m
     )
  => DAG.GrpcClient -> Store m
mkDagStore = DAG.mkDagStore


-- type aliases using the new, more elegant repr - will gradually phase it in
type Lazy m = M.Lazy m M
type Local = M.Local M
type PartialUpdate m = M.PartialUpdate m M
