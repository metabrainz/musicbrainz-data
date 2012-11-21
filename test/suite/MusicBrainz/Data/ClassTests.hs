module MusicBrainz.Data.ClassTests
    ( testCreateFindLatest, testMerge, testUpdate ) where

import Control.Applicative

import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Repository (acid2)

import MusicBrainz
import MusicBrainz.Data
import MusicBrainz.Data.Edit
import MusicBrainz.Data.Editor (register)

--------------------------------------------------------------------------------
testCreateFindLatest :: (Eq a, Show a, FindLatest a, Create a)
  => Tree a -> MusicBrainzT IO ()
testCreateFindLatest tree = do
  editor <- register acid2
  created <- create (entityRef editor) tree
  found <- findLatest (coreRef created)
  liftIO $ found @?= created


--------------------------------------------------------------------------------
testUpdate :: (Create a, Update a, FindLatest a, ViewTree a)
  => Tree a -> Tree a -> MusicBrainzT IO ()
testUpdate start end = do
  editor <- entityRef <$> register acid2

  created <- create editor start
  let entityId = coreRef created

  editId <- createEdit $
    update editor (coreRevision created) end

  apply editId

  found <- findLatest entityId
  latestTree <- viewTree (coreRevision found)
  liftIO $ latestTree @?= end

  parents <- revisionParents (coreRevision found)
  liftIO $
    coreRevision created `Set.member` parents @? "Is parented to starting revision"


--------------------------------------------------------------------------------
testMerge :: (RefMBID a, Merge a, Create a)
  => Tree a -> Tree a -> MusicBrainzT IO ()
testMerge treeA treeB = do
  editor <- entityRef <$> register acid2

  a <- create editor treeA
  b <- create editor treeB

  edit <- createEdit $
    merge editor (coreRevision a) (coreRef b)

  apply edit

  aResolved <- resolveMbid (refMbid $ coreRef a)
  liftIO $ aResolved @?= Just (coreRef b)
