import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import System.IO.Temp
import qualified Data.Map.Strict as M
import           Data.Functor.Foldable (cata, ana)
import           Data.Fix (Fix(..))
import           System.Directory as D
import           Control.Monad.Except
import           Control.Concurrent (threadDelay)
import           Merkle.App.BackingStore (BackingStore(..), buildStoreFromCtx)
import           Merkle.App.LocalState (initLocalState)
import           Merkle.App.Filesystem (buildCommitFromFilesystemState)
import           Merkle.App.Types
import Merkle.Bonsai.Types

import qualified Data.List.NonEmpty as NEL
import           Merkle.Generic.HRecursionSchemes

import Test.HUnit.Lang
import qualified System.Process as P


main :: IO ()
main = hspec $ do
  describe "Mononoke cmd line app" $ do
      it "initializes a repo and makes a commit" $ withDAGStore 8080 $ \testRoot' store -> do
            let testRoot = pure testRoot'
                ctx = BackingStore 8080 "localhost" -- TODO: shouldn't need this here
                toExpect = uncurry Change <$>
                      [( (NEL.fromList ["a", "b", "c"]), Add $ Term $ Blob "c")
                      ,( (NEL.fromList ["b", "c"]), Add $ Term $ Blob "c")
                      ,( (NEL.fromList ["c"]), Add $ Term $ Blob "c")
                      ,( (NEL.fromList ["d"]), Add $ Term $ Blob "d")
                      ]
            store <- buildStoreFromCtx ctx
            buildFT testRoot testFT
            initLocalState ctx testRoot
            res <- buildCommitFromFilesystemState store testRoot "first commit"
            liftIO $ res `shouldBe` toExpect


withDAGStore
   :: Port -- TODO: generate port, somehow
   -> (FilePath -> Store (ExceptT String IO) -> ExceptT String IO a) -- TODO: provide port via monad reader
   -> IO a
withDAGStore port action
  = withSystemTempDirectory "mononoke_test_root" $ \root -> do
      let dagStoreRoot = pure root <> (pure "dagstore")
      P.withCreateProcess
        (P.proc "../../run_test_dagstore" [concatPath dagStoreRoot, show port])
        { P.std_out = P.Inherit, P.std_err = P.Inherit }$
        \_stdin _stdout _stderr _ph -> do
            -- wait for app to start, lmao. todo: be better, lol. lol.
            threadDelay 1000000
            res <- runExceptT $ do
                      let ctx = BackingStore port "localhost"
                      store <- buildStoreFromCtx ctx
                      let testRoot = root <> "test_root"
                      -- ok so this _should_ be fine but the test seems to create the dir it uses as root? FIXME
                      -- liftIO $ createDirectory testRoot
                      action testRoot store
            either (assertFailure . ("test failed with err: " ++)) pure res




testFT :: FT
testFT = Fix $ Dir' $ M.fromList [("a", a), ("b", b), ("c", c), ("d", d)]
  where
    a = Fix $ Dir' $ M.fromList [("b", b)]
    b = Fix $ Dir' $ M.fromList [("c", c)]
    c = Fix $ File' "c"
    d = Fix $ File' "d"



buildFT
  :: forall m
   . ( MonadIO m
     , MonadError String m
     )
  => NEL.NonEmpty Path
  -> FT
  -> m ()
buildFT root ft = (cata f ft) []
  where
    concat path = concatPath $ maybe root (root <>) (NEL.nonEmpty path)

    f' path (pathSegment, action) =
      action $ path ++ [pathSegment]

    f :: FT' ([FilePath] -> m ()) -> ([FilePath] -> m ())
    f (Dir' xs) path = do
      liftIO $ D.createDirectory $ concat path
      mconcat <$> traverse (f' path) (M.toList xs)
    f (File' _) [] = -- throw if file entity at root dir
      throwError "file at root path of FT to build on filesystem"
    f (File' contents) path = do
      -- write FS state
      liftIO $ writeFile (concat path) contents




type FT = Fix FT'

data FT' a
  = File' String
  | Dir' (M.Map String a)
  deriving (Functor)
