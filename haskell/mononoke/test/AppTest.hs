{-# LANGUAGE RankNTypes #-}

import Control.Concurrent (threadDelay)
import Control.Monad.Except
import Control.Monad.Reader (runReaderT)
import Data.Fix (Fix (..))
import Data.Functor.Foldable (cata)
import qualified Data.List.NonEmpty as NEL
import Merkle.App.BackingStore (BackingStore (..), buildStoreFromCtx)
import Merkle.App.Filesystem (buildCommitFromFilesystemState)
import Merkle.App.Filesystem.Safe (MonadFileSystem (..), RootPath (..))
import Merkle.App.LocalState (initLocalState)
import Merkle.App.Types
import Merkle.Bonsai.Types
import Merkle.Generic.HRecursionSchemes
import System.Directory as D
import System.IO.Temp
import qualified System.Process as P
import qualified System.Random as R
import Test.HUnit.Lang
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Mononoke cmd line app" $ do
    it "initializes a repo and makes a commit" $
      withDAGStore $ \store -> do
        let toCreate = Fix $ Dir' [("a", a), ("b", b), ("c", c), ("d", d)]
            a = Fix $ Dir' [("b", b)]
            b = Fix $ Dir' [("c", c)]
            c = Fix $ File' "c"
            d = Fix $ File' "d"
        let toExpect =
              uncurry Change
                <$> [ ((NEL.fromList ["a", "b", "c"]), Add $ Term $ Blob "c"),
                      ((NEL.fromList ["b", "c"]), Add $ Term $ Blob "c"),
                      ((NEL.fromList ["c"]), Add $ Term $ Blob "c"),
                      ((NEL.fromList ["d"]), Add $ Term $ Blob "d")
                    ]
        RootPath testRoot <- rootPath
        buildFT (pure testRoot) toCreate
        initLocalState store
        res <- buildCommitFromFilesystemState store "first commit" []
        liftIO $ res `shouldBe` toExpect

randomPort :: IO Port
randomPort = R.getStdRandom $ R.randomR (16385, 65534)

withDAGStore ::
  forall a.
  Show a =>
  ( forall m.
    ( MonadIO m,
      MonadFileSystem m,
      MonadError String m
    ) =>
    Store m ->
    m a
  ) ->
  IO a
withDAGStore action =
  withSystemTempDirectory "mononoke_test_root" $ \root -> do
    let dagStoreRoot = pure root <> (pure "dagstore")
    port <- liftIO $ randomPort
    -- res <- P.withCreateProcess
    P.withCreateProcess
      (P.proc "cargo" ["run", "--bin", "dag-store", "--", "--fs_path", concatPath dagStoreRoot, "-p", show port])
        { P.std_out = P.Inherit,
          P.std_err = P.Inherit
        }
      $ \_stdin _stdout _stderr _ph -> do
        -- wait for app to start, lmao. todo: be better, lol. lol.
        threadDelay 1000000
        let testRoot = RootPath $ concatPath $ pure root <> pure "test_root"
        -- ok so this _should_ be fine but the test seems to create the dir it uses as root? FIXME
        -- TODO: oh right, the init fn creates a dir outside of the rootpath reader ctx
        -- liftIO $ createDirectory testRoot
        res <- flip runReaderT testRoot $
          runExceptT $ do
            let ctx = BackingStore port "localhost"
            store <- buildStoreFromCtx ctx
            action store
        either (assertFailure . ("test failed with err: " ++)) pure res

buildFT ::
  forall m.
  ( MonadIO m,
    MonadError String m
  ) =>
  NEL.NonEmpty Path ->
  FT ->
  m ()
buildFT root ft = (cata f ft) []
  where
    concat path = concatPath $ maybe root (root <>) (NEL.nonEmpty path)

    f' path (pathSegment, action) =
      action $ path ++ [pathSegment]

    f :: FT' ([FilePath] -> m ()) -> ([FilePath] -> m ())
    f (Dir' xs) path = do
      liftIO $ D.createDirectory $ concat path
      mconcat <$> traverse (f' path) xs
    f (File' _) [] =
      -- throw if file entity at root dir
      throwError "file at root path of FT to build on filesystem"
    f (File' contents) path = do
      -- write FS state
      liftIO $ writeFile (concat path) contents

type FT = Fix FT'

data FT' a
  = File' String
  | Dir' [(String, a)]
  deriving (Functor)
