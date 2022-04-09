module Merkle.Bonsai.Types.Examples where

import Data.List.NonEmpty (NonEmpty (..))
import Merkle.Bonsai.Types
import Merkle.Generic.HRecursionSchemes (Cxt (Term), Term)

-- ++ /a/foo foo
-- ++ /a/bar bar
-- ++ /baz baz
commit0 :: Term M 'CommitT
commit0 = Term $ Commit "c0: first commit" changes parents
  where
    changes = [c1, c2, c3]
    c1 = add ("a" :| ["foo"]) . Term $ Blob "foo"
    c2 = add ("a" :| ["bar"]) . Term $ Blob "bar"
    c3 = add ("baz" :| []) . Term $ Blob "baz"
    parents = Term NullCommit :| []

-- ++ /.DS_Store jk
-- -- /a/bar
commit1 :: Term M 'CommitT
commit1 = Term $ Commit "c1: askdfj" changes parents
  where
    changes = [c1, c2, c3]
    c1 = add (".DS_Store" :| []) . Term $ Blob "..."
    c2 = del ("a" :| ["bar"])
    c3 = add ("baz" :| []) . Term $ Blob "baz with new content"
    parents = commit0 :| []

-- ++ /README todo
commit2 :: Term M 'CommitT
commit2 = Term $ Commit "c2: todo: readme" changes parents
  where
    changes = [c1]
    c1 = add ("README" :| []) . Term $ Blob "todo"
    parents = commit0 :| []

-- TODO: should this have 'bar', deleted in 1/2 of parent commits and not changed in this commit?
-- TODO: I think that should have to be resolved (file present in one, absent in other - maybe?)
commit3 :: Term M 'CommitT
commit3 = Term $ Commit "c3: merge" [resolvingChange] parents
  where
    parents = commit1 :| [commit2]
    resolvingChange = add ("baz" :| []) . Term $ Blob "baz3"
