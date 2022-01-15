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
import qualified Merkle.App as App
import Merkle.Bonsai.Types

import qualified Data.List.NonEmpty as NEL
import           Merkle.Generic.HRecursionSchemes

import Test.HUnit.Lang
import qualified System.Process as P


main :: IO ()
main = hspec $ do
  describe "Mononoke cmd line app" $ do
      let runTest = withSystemTempDirectory "mononoke_test_root"
      it "initializes a repo and makes a commit" $ runTest testInitCommit



testInitCommit :: FilePath -> IO ()
testInitCommit root = wrapper (a >>= expect)
  where
    -- NOTE: with regard to directory run in, lol
    wrapper action = P.withCreateProcess (P.proc "../../run_test_dagstore" [concatPath dagStoreRoot, show port]) { P.std_out = P.Inherit, P.std_err = P.Inherit } $
                   \_stdin _stdout _stderr _ph -> do
                       -- wait for app to start, lmao. todo: be better, lol. lol.
                       threadDelay 1000000
                       action

    testRoot, dagStoreRoot :: NEL.NonEmpty Path
    testRoot = pure root <> (pure "test")
    dagStoreRoot = pure root <> (pure "dagstore")
    port = 8080
    expect = shouldBe toExpect
    toExpect = uncurry Change <$>
           [( (NEL.fromList ["a", "b", "c"]), Add $ Term $ Blob "c")
           ,( (NEL.fromList ["b", "c"]), Add $ Term $ Blob "c")
           ,( (NEL.fromList ["c"]), Add $ Term $ Blob "c")
           ,( (NEL.fromList ["d"]), Add $ Term $ Blob "d")
           ]
    a = do
      res <- runExceptT $ do
            liftIO $ putStrLn "prebuild"
            buildFT testRoot testFT
            liftIO $ putStrLn "postbuild"
            App.initLocalState "localhost" 8080 testRoot
            liftIO $ putStrLn "postinit"
            -- ls <- App.readLocalState (pure root)
            App.commit testRoot "first commit"

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
