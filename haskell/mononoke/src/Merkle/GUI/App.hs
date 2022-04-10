{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Merkle.GUI.App where

--------------------------------------------
import qualified Clay as Clay
import Control.Concurrent.STM
import Control.Monad (void)
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty, toList)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Singletons.TH (SingI, sing)
import Data.Text.Lazy (unpack)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Ext.Flexbox
--------------------------------------------
import Merkle.App.LocalState
import Merkle.Bonsai.MergeTrie
import Merkle.Bonsai.Types
import Merkle.Bonsai.Types.Tags (typeTagName')
import Merkle.GUI.CSS
import Merkle.GUI.Core
import Merkle.GUI.Elements
import qualified Merkle.GUI.Modal as Modal
import Merkle.GUI.State
import Merkle.Generic.BlakeHash
import Merkle.Generic.HRecursionSchemes
import Merkle.Generic.Merkle (commitPartialUpdate, fetchLazy, fetchLazyT, lazyExpandHash, newStructure, oldStructure)
--------------------------------------------
import Optics ((^.))

type Minimizations = Set RawBlakeHash

branchBrowser ::
  Index UI ->
  Store UI ->
  Handler (Modal.SpawnPopup UI) ->
  BranchState UI ->
  Handler (FocusLazy UI) ->
  Handler BranchFocus ->
  UI Element
branchBrowser commitSnapshotIndex store _popRequest bs focusChangeHandler updateBranchStateHandler = do
  let extraBranches = (\(f, c) -> (OtherBranch f, c)) <$> bsBranches bs
  faUl #+ (fmap drawBranch $ [(MainBranch, bsMainBranch bs)] ++ extraBranches)
  where
    drawBranch (f, commit) = do
      esnap <- runExceptT $ updateSnapshotIndexLazy store commitSnapshotIndex commit

      let commit' = faLi focusChangeHandler commit [] (string "commit: ") UI.div
          snap' = case esnap of
            Right snap -> faLi focusChangeHandler snap [] (string "snap: ") UI.div
            Left e -> string $ "unable to construct snapshot: " ++ show e

      let extraTags = if f == bsFocus bs then ["focus", "branch"] else ["branch"]
      let extraActions =
            if f == bsFocus bs
              then []
              else [("fa-search", liftIO $ updateBranchStateHandler f)]

      case f of
        MainBranch -> do
          faLiSimple extraTags "fa-code-branch" extraActions (string "main branch") $ faUl #+ [commit', snap']
        OtherBranch branchName -> do
          faLiSimple
            extraTags
            "fa-code-branch"
            extraActions
            (string branchName)
            $ faUl #+ [commit', snap']

-- construct a NEL from a list and an element,
-- but with the element appended to the list instead of prepended
appendNEL :: [a] -> a -> NonEmpty a
appendNEL xs x = maybe (pure x) (<> pure x) $ nonEmpty xs

parsePath :: String -> Maybe (NonEmpty Path)
parsePath s = do
  case nonEmpty (filter (/= "") $ splitOn "/" s) of
    Nothing -> Nothing
    Just xs -> Just xs

browseLazyTerm ::
  Handler (FocusLazy UI) ->
  TVar Minimizations ->
  FocusLazy UI ->
  UI Element
browseLazyTerm focusHandler minimizations focus = case focus of
  SnapshotF root -> (getConst $ hpara (browseLazy focusHandler minimizations) root)
  FileTreeF root -> (getConst $ hpara (browseLazy focusHandler minimizations) root)
  CommitF root -> (getConst $ hpara (browseLazy focusHandler minimizations) root)
  BlobF root -> (getConst $ hpara (browseLazy focusHandler minimizations) root)

browseLazy ::
  Handler (FocusLazy UI) ->
  TVar Minimizations ->
  RAlg (Lazy UI) (Const (UI Element))
browseLazy focusHandler minimizations lazy = Const $ do
  m <- fetchLazy lazy
  let action = liftIO $ focusHandler $ wrapFocus sing $ Term $ hfmap _tag lazy
  getConst $ browseMononoke minimizations action ["persisted"] $ hfmap _elem m

browseMononoke ::
  forall (i :: MTag).
  SingI i =>
  TVar Minimizations ->
  UI () ->
  [String] ->
  Local (Const (UI Element)) i ->
  (Const (UI Element)) i
browseMononoke minimizations focusAction extraTags local = Const $ do
  content <- UI.div # withClass ([typeTagName' m] ++ extraTags) #+ [browseMononoke' m]
  faLi' @i
    (Just focusAction)
    [ ("fa-eye", toggleNode (getConst h) content)
    ]
    (string $ typeTagName' m)
    (pure content)
  where
    h = local ^. #hash
    m = local ^. #node

    toggleNode :: RawBlakeHash -> Element -> UI ()
    toggleNode k node = void $ do
      isMinimized <- liftIO $
        atomically $ do
          wasMinimized <- Set.member k <$> readTVar minimizations
          if wasMinimized
            then do
              modifyTVar minimizations (Set.delete k)
            else do
              modifyTVar minimizations (Set.insert k)
          pure $ not wasMinimized

      if isMinimized
        then element node # set UI.class_ "hidden"
        else element node # set UI.class_ ""

    renderChange :: Change (Const (UI Element)) -> UI Element
    renderChange Change {..} =
      let renderPath = mconcat . intersperse "/" . toList
       in case _change of
            Add (Const next) ->
              faLiSimple ["add"] "fa-plus-circle" [] (string $ "Add: " ++ renderPath _path) $ faUl #+ [next]
            Del -> faLiSimple ["del"] "fa-minus-circle" [] (string $ "Del: " ++ renderPath _path) UI.div

    -- returns fa-ul item or div/string/etc
    browseMononoke' :: forall (x :: MTag). M (Const (UI Element)) x -> UI Element
    browseMononoke' (Snapshot t o ps) =
      (faUl #+) $
        [ faLiSimple' [] "fa-chevron-right" (string "root") $ getConst t,
          faLiSimple' [] "fa-chevron-right" (string "generated from commit") $ getConst o
        ]
          ++ (faLiSimple' [] "fa-chevron-right" (string "parent snapshot") . getConst <$> ps)
    browseMononoke' (File (SnapshotFile b l p)) =
      (faUl #+) $
        [ faLiSimple' [] "fa-chevron-right" (string "blob") $ getConst b,
          faLiSimple' [] "fa-chevron-right" (string "last modified") $ getConst l
        ]
          ++ (faLiSimple' [] "fa-chevron-right" (string "previous incarnation") . getConst <$> p)
    browseMononoke' (Dir cs) =
      (faUl #+) $
        let renderChild (k, Const v) = faLiSimple' [] "fa-chevron-right" (string k) v
         in renderChild <$> Map.toList cs
    browseMononoke' NullCommit = string "NullCommit"
    browseMononoke' (Commit msg cs ps) =
      (faUl #+) $
        [faLiSimple' [] "fa-comment-alt" (string $ "msg: " ++ msg) UI.div]
          ++ (renderChange <$> cs)
          ++ (faLiSimple' [] "fa-chevron-right" (string "parent") . getConst <$> toList ps)
    browseMononoke' (Blob c) = UI.string $ "\"" ++ c ++ "\""

mononokeGUI :: LocalState -> Store UI -> IO ()
mononokeGUI localstate store = do
  -- TODO/FIXME - make this work on other people's machines by, idk, packaging static files or similar (or just an env var lol)
  startGUI (defaultConfig {jsStatic = Just "/home/inanna/hexacorallia/haskell/mononoke/static"}) (setup localstate store)

-- NOTE/TODO: this could all be in a single STM transaction. wild.
updateSnapshotIndexLazy ::
  MonadIO m =>
  Store m ->
  Index m ->
  Term (Lazy m) 'CommitT ->
  ExceptT (NonEmpty MergeError) m (Term (Lazy m) 'SnapshotT)
updateSnapshotIndexLazy store index lazyCommitT = do
  msnap <- lift $ (iRead index) (unTerm lazyCommitT ^. #hash)
  localCommit <- lift $ fetchLazyT lazyCommitT
  case msnap of
    Just h -> do
      pure $ lazyExpandHash (sRead store) h
    Nothing -> do
      snap <- makeSnapshot (hfmap oldStructure $ localCommit ^. #node) (iRead index) (sRead store)
      let wipt = newStructure snap
      -- NOTE: this is the only place that writes occur
      uploadedSnap <- lift $ commitPartialUpdate (sWrite store) wipt
      lift $ (iWrite index) (localCommit ^. #hash) (unTerm uploadedSnap ^. #hash)
      pure uploadedSnap

setup :: LocalState -> Store UI -> Window -> UI ()
setup localstate store root = void $ do
  _ <- set UI.title "chibi mononoke!" (pure root)
  void $
    getHead root
      #+ [ mkElement "style" # set (UI.text) (unpack (Clay.render css))
         ]
  UI.addStyleSheet root "all.css" -- fontawesome
  UI.addStyleSheet root "aesthetic.css" -- vaporwave! https://github.com/torch2424/aesthetic-css/blob/master/aesthetic.css
  commitSnapshotIndexTVar <- liftIO . atomically $ newTVar Map.empty
  let commitSnapshotIndex = stmIOIndex commitSnapshotIndexTVar

  let initialBranchState = fromLocalState store localstate

  branchState <- liftIO . atomically $ newTVar initialBranchState
  minimizations <- liftIO . atomically $ newTVar (Set.empty)

  (updateBranchStateEvent, updateBranchStateHandler) <- liftIO $ newEvent
  (focusChangeEvent, focusChangeHandler) <- liftIO $ newEvent
  (popupEvent, popupHandler) <- liftIO $ newEvent

  -- empty root nodes for various UI elements
  sidebarRoot <- infraDiv # withClass ["sidebar"]
  modalRoot <- UI.div -- not infra because often invisible, infra controls display

  -- TODO: too useful to nuke (also, it's v. cool - windows-look UI and everything)
  let _popError :: NonEmpty MergeError -> UI ()
      _popError e = liftIO $ popupHandler $ Modal.SpawnError $ show e

  let redrawSidebar bs = void $ do
        _ <- element sidebarRoot # set children []
        element sidebarRoot
          #+ [ UI.div # withClass ["placeholder"],
               branchBrowser commitSnapshotIndex store popupHandler bs focusChangeHandler updateBranchStateHandler
             ]

  -- discarded return value deregisters handler
  _ <- onEvent popupEvent (Modal.handleSpawnPopup modalRoot)

  browserRoot <- faUl
  -- discarded return value deregisters handler
  _ <- onEvent focusChangeEvent $ \focus -> do
    void $ element browserRoot # set children []
    void $ element browserRoot #+ [browseLazyTerm focusChangeHandler minimizations focus]
    pure ()

  liftIO $ focusChangeHandler $ wrapFocus sing (bsFocusedCommit initialBranchState)

  _ <- onEvent updateBranchStateEvent $ \ubs -> do
    bs' <- case ubs of
      focus -> do
        liftIO $ putStrLn "changefocus"
        liftIO $
          atomically $ do
            bs <- readTVar branchState
            -- TODO: error if focus not found in list of branches
            let bs' = bs {bsFocus = focus}
            writeTVar branchState bs'
            pure bs'

    redrawSidebar bs'
    pure ()

  bs <- liftIO $ atomically $ readTVar branchState

  redrawSidebar bs

  void $
    flex_p
      (getBody root)
      [ (element sidebarRoot, flexGrow 1),
        ( infraDiv #+ [element browserRoot],
          flexGrow 2
        )
      ]
  void $ getBody root #+ [element modalRoot]
