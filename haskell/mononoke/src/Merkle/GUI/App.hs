{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Merkle.GUI.App where

--------------------------------------------
import qualified Clay as Clay
import           Control.Concurrent.STM
import           Control.Monad (void)
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Default
import           Data.List (intersperse)
import           Data.List.NonEmpty (toList, nonEmpty, (<|), NonEmpty(..))
import           Data.List.Split (splitOn)
import           Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Text.Lazy (unpack)
import           Data.Singletons.TH (SingI, sing)
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Ext.Flexbox
--------------------------------------------
import           Merkle.Bonsai.Types
import qualified Merkle.Bonsai.Types.Examples as Examples
import           Merkle.Bonsai.Types.Tags (typeTagName, typeTagName')
import           Merkle.Bonsai.MergeTrie
import           Merkle.GUI.CSS
import           Merkle.GUI.Core
import           Merkle.GUI.Elements
import           Merkle.GUI.State
import qualified Merkle.GUI.Modal as Modal
import qualified Merkle.GUI.WorkingMergeTrie as WorkingMergeTrie
import           Merkle.GUI.Messages
import           Merkle.Generic.BlakeHash
import           Merkle.Generic.HRecursionSchemes
--------------------------------------------

type Minimizations = Set RawBlakeHash


branchBrowser
  :: Index UI
  -> Store UI
  -> Handler (SpawnPopup UI)
  -> BranchState UI
  -> Handler (FocusWIPT UI)
  -> Handler UpdateBranchState
  -> UI Element
branchBrowser commitSnapshotIndex store popRequest bs focusChangeHandler updateBranchStateHandler = do
    let extraBranches = (\(f,c) -> (OtherBranch f, c)) <$> bsBranches bs
    faUl #+ (fmap drawBranch $ [(MainBranch, bsMainBranch bs)] ++ extraBranches)

  where
    drawBranch (f, commit) = do
      esnap <- runExceptT $ updateSnapshotIndexLMMT store commitSnapshotIndex commit

      let commit' = faLi focusChangeHandler (unmodifiedWIP commit) [] (string "commit: ") UI.div
          snap'   = case esnap of
            Right snap -> faLi focusChangeHandler (unmodifiedWIP snap) [] (string "snap: ") UI.div
            Left e ->     string $ "unable to construct snapshot: " ++ show e

      let extraTags = if f == bsFocus bs then ["focus", "branch"] else ["branch"]
          forkAction focus = do
            liftIO $ popRequest $ SpawnRequestText "branch name" $ \newBranchName ->
              liftIO $ updateBranchStateHandler $ ForkFrom focus newBranchName
      let extraActions = if f == bsFocus bs
            then [("fa-code-branch", forkAction f)]
            else [("fa-code-branch", forkAction f), ("fa-search", liftIO $ updateBranchStateHandler $ ChangeFocus f)]

      case f of
        MainBranch    -> do
          faLiSimple extraTags "fa-code-branch" extraActions (string "main branch") $ faUl #+ [commit', snap']
        OtherBranch branchName -> do
          let delAction = liftIO $ do
                putStrLn $ "delete branch w/ name " ++ branchName
                updateBranchStateHandler $ DelBranch branchName
          faLiSimple extraTags "fa-code-branch" ([("fa-trash-alt", delAction)] ++ extraActions)
                                         (string branchName) $ faUl #+ [commit', snap']



renderWIPTBlob :: Handler (FocusWIPT UI) -> [(String, UI ())] -> WIPT UI 'BlobT -> UI Element
renderWIPTBlob focusHandler actions wipt = do
  let extraTags = case wipt of
        (Term (HC (L _))) -> ["persisted"]
        (Term (HC (R _))) -> ["wip"]

  (HC (Tagged _h blob)) <- fetchWIPT wipt
  case blob of
    Blob b -> do
      body <- UI.div # withClass (extraTags ++ [typeTagName $ sing @'BlobT])
                    #+ [string $ "\"" ++ b ++ "\""]
      faLi focusHandler wipt actions (string "file blob")
                                      (element body)




-- construct a NEL from a list and an element,
-- but with the element appended to the list instead of prepended
appendNEL :: [a] -> a -> NonEmpty a
appendNEL xs x = maybe (pure x) (<> pure x) $ nonEmpty xs


drawCommitEditor
  :: BranchState UI
  -> Handler (SpawnPopup UI)
  -> Handler (UpdateMergeTrie UI)
  -> Handler (FocusWIPT UI)
  -> Maybe (InProgressCommit UI)
  -> UI Element
drawCommitEditor bs popRequest modifyMergeTrieHandler focusHandler ipc = do

  viewCommit <- case ipc of
    Nothing -> string "no in progress commit"
    _ -> error "TODO remove"

  UI.div #+ [ element viewCommit
            , UI.br
            , string "branches:"
            ]

  where

    renderChange :: NonEmpty Path -> ChangeType (WIPT UI) -> UI Element
    renderChange p (Add wipt) =
      faLiSimple ["add"] "fa-plus-circle" (removeChange p) (string $ "Add: " ++ renderPath p) $ faUl #+ [renderWIPTBlob focusHandler [] wipt]
    renderChange p Del =
      faLiSimple ["del"] "fa-minus-circle" (removeChange p) (string $ "Del: " ++ renderPath p) UI.div

    removeChange nel = [("fa-trash-alt", liftIO $ modifyMergeTrieHandler $ RemoveChange nel)]



    viewChanges cs = do
      faUl #+ (uncurry renderChange <$> (Map.toList cs))

    renderPath = mconcat . intersperse "/" . toList



parsePath :: String -> Maybe (NonEmpty Path)
parsePath s = do
  case nonEmpty (filter (/= "") $ splitOn "/" s) of
    Nothing -> Nothing
    Just xs -> Just xs


browseWIPT
  :: Handler (FocusWIPT UI)
  -> TVar Minimizations
  -> FocusWIPT UI
  -> UI Element
browseWIPT focusHandler minimizations focus = case focus of
    SnapshotF root -> (getConst $ hpara (uiWIPAlg focusHandler minimizations) root)
    FileTreeF root -> (getConst $ hpara (uiWIPAlg focusHandler minimizations) root)
    CommitF   root -> (getConst $ hpara (uiWIPAlg focusHandler minimizations) root)
    BlobF     root -> (getConst $ hpara (uiWIPAlg focusHandler minimizations) root)


browseLMMT
  :: Handler (FocusWIPT UI)
  -> TVar Minimizations
  -> FocusLMMT UI
  -> UI Element
browseLMMT focusHandler minimizations focus = case focus of
    SnapshotF root -> (getConst $ hpara (uiLMMAlg focusHandler minimizations) root)
    FileTreeF root -> (getConst $ hpara (uiLMMAlg focusHandler minimizations) root)
    CommitF   root -> (getConst $ hpara (uiLMMAlg focusHandler minimizations) root)
    BlobF     root -> (getConst $ hpara (uiLMMAlg focusHandler minimizations) root)



uiWIPAlg
  :: Handler (FocusWIPT UI)
  -> TVar Minimizations
  -> RAlg (WIP UI) (Const (UI Element))
uiWIPAlg focusHandler minimizations (HC (L lmmt)) = Const $ do
      browseLMMT focusHandler minimizations (wrapFocus sing $ lmmt)
uiWIPAlg focusHandler minimizations wipt@(HC (R m)) = Const $ do
      let action = liftIO $ focusHandler $ wrapFocus sing $ Term $ hfmap _tag wipt
      getConst $ browseMononoke minimizations action ["wip"] $ hfmap _elem m


uiLMMAlg
  :: Handler (FocusWIPT UI)
  -> TVar Minimizations
  -> RAlg (LMM UI) (Const (UI Element))
uiLMMAlg focusHandler minimizations lmm = Const $ do
      m <-  fetchLMM lmm
      let action = liftIO $ focusHandler $ wrapFocus sing $ unmodifiedWIP $ Term $ hfmap _tag lmm
      getConst $ browseMononoke minimizations action ["persisted"] $ hfmap _elem m


browseMononoke
  :: forall (i :: MTag)
   . SingI i
  => TVar Minimizations
  -> UI ()
  -> [String]
  -> (Tagged Hash `HCompose` M) (Const (UI Element)) i
  -> (Const (UI Element)) i
browseMononoke minimizations focusAction extraTags (HC (Tagged h m)) = Const $ do
      content <- UI.div # withClass ([typeTagName' m] ++ extraTags) #+ [browseMononoke' m]
      faLi' @i (Just focusAction) [ ("fa-eye", toggleNode (getConst h) content)
                                  ]
                                  (string $ typeTagName' m) (pure content)

  where
    toggleNode :: RawBlakeHash -> Element -> UI ()
    toggleNode k node = void $ do
      isMinimized <- liftIO $ atomically $ do
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
    renderChange Change{..} =
        let renderPath = mconcat . intersperse "/" . toList
        in case _change of
              Add (Const next) ->
                faLiSimple ["add"] "fa-plus-circle" [] (string $ "Add: " ++ renderPath _path) $ faUl #+ [next]
              Del -> faLiSimple ["del"] "fa-minus-circle" [] (string $ "Del: " ++ renderPath _path) UI.div



    -- returns fa-ul item or div/string/etc
    browseMononoke' :: forall (x:: MTag). M (Const (UI Element)) x -> UI Element
    browseMononoke' (Snapshot t o ps) = (faUl #+) $
      [ faLiSimple' [] "fa-chevron-right" (string "root") $ getConst t
      , faLiSimple' [] "fa-chevron-right" (string "generated from commit") $ getConst o
      ] ++
      ( faLiSimple' [] "fa-chevron-right" (string "parent snapshot") . getConst <$> ps)

    browseMononoke' (File (SnapshotFile b l p)) = (faUl #+) $
      [ faLiSimple' [] "fa-chevron-right" (string "blob") $ getConst b
      , faLiSimple' [] "fa-chevron-right" (string "last modified") $ getConst l
      ] ++ (faLiSimple' [] "fa-chevron-right" (string "previous incarnation") . getConst <$>  p)

    browseMononoke' (Dir cs) = (faUl #+) $
          let renderChild (k, Const v) = faLiSimple' [] "fa-chevron-right" (string k) v
           in renderChild <$> Map.toList cs

    browseMononoke' NullCommit = string "NullCommit"

    browseMononoke' (Commit msg cs ps) = (faUl #+) $
      [ faLiSimple' [] "fa-comment-alt" (string $ "msg: " ++ msg) UI.div ] ++
      (renderChange <$> cs) ++
      ( faLiSimple' [] "fa-chevron-right" (string "parent") . getConst <$> toList ps)

    browseMononoke' (Blob c) =  UI.string $ "\"" ++ c ++ "\""


mononokeGUI :: IO ()
mononokeGUI = do
  startGUI (defaultConfig { jsStatic = Just "static"}) setup


-- NOTE/TODO: this could all be in a single STM transaction. wild.
updateSnapshotIndexLMMT
  :: MonadIO m
  => Store m
  -> Index m
  -> LMMT m 'CommitT
  -> ExceptT (NonEmpty MergeError) m (LMMT m 'SnapshotT)
updateSnapshotIndexLMMT store index commit = do
  msnap <- lift $ (iRead index) (hashOfLMMT commit)
  (HC (Tagged _ commit')) <- lift $ fetchLMMT commit
  case msnap of
    Just h  -> do
      pure $ expandHash (sRead store) h
    Nothing -> do
      snap <- makeSnapshot (hfmap unmodifiedWIP commit') (iRead index) (sRead store)
      let wipt = modifiedWIP snap
      uploadedSnap <- lift $ uploadWIPT (sWrite store) wipt
      lift $ (iWrite index) (hashOfLMMT commit) (hashOfLMMT uploadedSnap)
      pure uploadedSnap


updateSnapshotIndexWIPT
  :: MonadIO m
  => StoreRead m
  -> IndexRead m
  -> WIPT m 'CommitT
  -> ExceptT (NonEmpty MergeError) m (M (WIPT m) 'SnapshotT)
updateSnapshotIndexWIPT store index commit = do
  (HC (Tagged _ commit')) <- lift $ fetchWIPT commit
  makeSnapshot commit' index store


setup :: Window -> UI ()
setup root = void $ do
  _ <- set UI.title "chibi mononoke!" (pure root)
  void $ getHead root #+ [ mkElement "style" # set (UI.text) (unpack (Clay.render css))
                         ]
  UI.addStyleSheet root "all.css" -- fontawesome
  UI.addStyleSheet root "aesthetic.css" -- vaporwave! https://github.com/torch2424/aesthetic-css/blob/master/aesthetic.css


  commitSnapshotIndexTVar <- liftIO . atomically $ newTVar Map.empty
  let commitSnapshotIndex = stmIOIndex commitSnapshotIndexTVar

  inProgressCommitTVar :: TVar (Maybe (InProgressCommit UI))
    <- liftIO . atomically $ newTVar $ Nothing

  blobStoreTvar <- liftIO . atomically $ newTVar emptyBlobStore
  let blobStore = stmIOStore blobStoreTvar

  initCommit <- expandHash (sRead blobStore) <$> uploadM (sWrite blobStore) Examples.commit1
  branchCommit <- expandHash (sRead blobStore) <$> uploadM (sWrite blobStore) Examples.commit2

  let initialBranchState = BranchState
                         { bsMainBranch = initCommit
                         , bsBranches = [("branch", branchCommit)]
                         , bsFocus = MainBranch
                         }


  branchState <- liftIO . atomically $ newTVar initialBranchState
  minimizations <- liftIO . atomically $ newTVar (Set.empty)

  let updateCurrentBranch :: LMMT UI 'CommitT -> STM ()
      updateCurrentBranch commit = do
        bs <- readTVar branchState
        let focus = bsFocus bs
        case focus of
          MainBranch -> do
            writeTVar branchState $ bs { bsMainBranch = commit}
          OtherBranch b -> do
            let update (s,c) | s == b    = (s, commit) -- update commit if exists (janky? maybe, idk)
                             | otherwise = (s,c)
            writeTVar branchState $ bs { bsBranches = update <$> bsBranches bs}

  (updateBranchStateEvent, updateBranchStateHandler) <- liftIO $ newEvent
  (focusChangeEvent, focusChangeHandler) <- liftIO $ newEvent
  (modifyMergeTrieEvent, modifyMergeTrieHandler) <- liftIO $ newEvent
  (popupEvent, popupHandler) <- liftIO $ newEvent


  -- empty root nodes for various UI elements
  sidebarRoot <- infraDiv # withClass ["sidebar"]
  modalRoot <- UI.div -- not infra because often invisible, infra controls display


  let popError :: NonEmpty MergeError -> UI ()
      popError e = liftIO $ popupHandler $ SpawnError $ show e

  let extractFT :: M x 'SnapshotT -> x 'FileTree
      extractFT (Snapshot ft _ _) = ft


  -- errors on key not found - not sure how to handle this, need
  -- generic 'shit broke' error channel, eg popup/alert
  -- TODO: need to run everything in exceptT so I can pop an error when shit like this happens
  let getCurrentBranch :: STM (BranchFocus, LMMT UI 'CommitT)
      getCurrentBranch = do
        bs <- readTVar branchState
        let focus = bsFocus bs
        case focus of
          MainBranch -> pure $ (focus, bsMainBranch bs)
          OtherBranch b -> maybe undefined (pure . (focus,)) $ lookup b (bsBranches bs)

  let redrawSidebar bs mipc = void $ do
        _ <- element sidebarRoot # set children []
        element sidebarRoot #+ [ drawCommitEditor bs popupHandler modifyMergeTrieHandler focusChangeHandler mipc
                               , branchBrowser commitSnapshotIndex blobStore popupHandler bs focusChangeHandler updateBranchStateHandler
                               ]

  let handleErr m = do
        x <- runExceptT m
        case x of
          Right () -> pure ()
          Left e   -> popError e

      handleMMTE :: UpdateMergeTrie UI -> UI ()
      handleMMTE msg = handleErr $ pure ()


  -- discarded return value deregisters handler
  _ <- onEvent popupEvent (Modal.handleSpawnPopup modalRoot)

  -- discarded return value deregisters handler
  _ <- onEvent modifyMergeTrieEvent handleMMTE

  -- TODO: only reset on DelBranch if focus changed (current branch deleted == auto-reset)
  liftIO $ modifyMergeTrieHandler $ Reset

  browserRoot <- faUl
  -- discarded return value deregisters handler
  _ <- onEvent focusChangeEvent $ \focus -> do
    void $ element browserRoot # set children []
    void $ element browserRoot #+ [browseWIPT focusChangeHandler minimizations focus]
    pure ()


  liftIO $ focusChangeHandler $ wrapFocus sing $ unmodifiedWIP initCommit

  _ <- onEvent updateBranchStateEvent $ \ubs -> do
    bs' <- case ubs of
      ChangeFocus f -> do
        liftIO $ putStrLn "changefocus"
        liftIO $ atomically $ do
          -- TODO: check in progress commit, if set -> error
          bs <- readTVar branchState
          -- TODO: error if focus not found in list of branches
          let bs' = bs { bsFocus = f }
          writeTVar branchState bs'
          pure bs'
      _ -> error "only changing focus now supported"

    -- dispatch reset after change focus - will redraw merge trie & reset + redraw commit editor
    case ubs of
      _ -> -- FIXME: just run in all cases b/c that'll trigger redraw of commit editor - could optimize more here
        liftIO $ modifyMergeTrieHandler Reset

    redrawSidebar bs' Nothing
    pure ()

  bs <- liftIO $ atomically $ readTVar branchState

  redrawSidebar bs Nothing

  void $ flex_p (getBody root)
                [ (element sidebarRoot, flexGrow 1)
                , ( infraDiv #+ [element browserRoot]
                  , flexGrow 2
                  )
                ]
  void $ getBody root #+ [element modalRoot]

