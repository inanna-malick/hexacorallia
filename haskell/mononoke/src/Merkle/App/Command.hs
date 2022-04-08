{-# LANGUAGE TypeFamilies #-}
{-# language ScopedTypeVariables        #-}


module Merkle.App.Command where

import           Merkle.App.BackingStore (BackingStore(..))
import           Merkle.App.Types (Message, BranchName)
import           Options.Applicative


appCommand :: Parser (BackingStore, Command)
appCommand = (,) <$> ctx <*> cmd
  where
    ctx = BackingStore
      <$> option auto
          ( long "port"
         <> metavar "Port"
         <> showDefault
         <> value 8088
         <> help "DAG store port" )
      <*> strOption
          ( long "addr"
         <> metavar "Addr"
         <> showDefault
         <> value "localhost"
         <> help "DAG store address" )

    branchName = argument str (metavar "BRANCH_NAME")

    commitArgs = CreateCommit
             <$> branchName
             <*> many (argument str (metavar "MERGE..."))

    cmd = subparser
      ( command "diff" (info (pure ShowUncommitedChanges) ( progDesc "show uncommited changes"))
     <> command "commit" (info commitArgs ( progDesc "commit changes" ))
     <> command "create_branch" (info (CreateBranch <$> branchName) ( progDesc "create a new branch"))
     <> command "delete_branch" (info (DeleteBranch <$> branchName) ( progDesc "delete an existing branch"))
     <> command "checkout" (info (CheckoutBranch <$> branchName)
                             ( progDesc "checkout an existing branch. fails if uncommited changes")
                           )
     <> command "init" (info (pure InitializeRepo) ( progDesc "init a new repo in current dir"))
     <> command "gui" (info (pure StartGUI) ( progDesc "launch gui for repo" ))
      )


-- diff: show extant changes (basically just commit dry run, super easy kinda )
-- all commands except for Init must be run in the root dir of an initialized repo
data Command
  = StartGUI
  | InitializeRepo -- init repo in current dir
  | CreateCommit Message [BranchName] -- commit all diffs in current dir, merging the provided branches if any
  | ShowUncommitedChanges -- show diffs in current dir
  | CreateBranch BranchName-- create a branch
  | DeleteBranch BranchName-- create a branch
  | CheckoutBranch BranchName -- checkout an existing branch, imposing it on the current repo
-- NOTE: no need for snapshot browser/blame/etc, that's all via the web UI

