{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module MusicBrainz.Data.ClassTests
    ( testAliases
    , testAnnotation
    , testCreateFindLatest
    , testMerge
    , testUpdate
    ) where

import Control.Applicative
import Control.Lens

import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Repository (acid2)

import MusicBrainz
import MusicBrainz.Lens
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
testMerge :: (RefSpec a ~ MBID a, Merge a, Referenceable a, ResolveReference a, Create a)
  => Tree a -> Tree a -> MusicBrainzT IO ()
testMerge treeA treeB = do
  editor <- entityRef <$> register acid2

  a <- create editor treeA
  b <- create editor treeB

  edit <- createEdit $
    merge editor (coreRevision a) (coreRef b)

  apply edit

  aResolved <- resolveReference (dereference $ coreRef a)
  liftIO $ aResolved @?= Just (coreRef b)


--------------------------------------------------------------------------------
testAliases :: (Create a, FindLatest a, TreeAliases a, Update a, ViewAliases a) => Tree a -> Alias -> MusicBrainzT IO ()
testAliases tree alias = do
  editor <- entityRef <$> register acid2

  artist <- create editor (aliases .~ Set.singleton alias $ tree)
  aliasesPreUpdate <- viewAliases (coreRevision artist)
  liftIO $ aliasesPreUpdate @?= Set.singleton alias

  edit <- createEdit $
    update editor (coreRevision artist) tree

  apply edit

  latest <- findLatest (coreRef artist)
  aliasesPostUpdate <- viewAliases (coreRevision latest)
  liftIO $ aliasesPostUpdate @?= Set.empty


--------------------------------------------------------------------------------
testAnnotation :: (Create a, FindLatest a, TreeAnnotation a, Update a, ViewAnnotation a)
  => (Ref Editor -> MusicBrainz (Tree a)) -> MusicBrainz ()
testAnnotation startTree = do
  editor <- entityRef <$> register acid2

  entityTree <- startTree editor
  entity <- create editor (annotation .~ expected $ entityTree)
  annPreUpdate <- viewAnnotation (coreRevision entity)
  liftIO $ annPreUpdate @?= expected

  edit <- createEdit $
    update editor (coreRevision entity) entityTree

  apply edit

  latest <- findLatest (coreRef entity)
  annPostUpdate <- viewAnnotation (coreRevision latest)
  liftIO $ annPostUpdate @?= ""

  where
    expected = "This is the expected annotation"
