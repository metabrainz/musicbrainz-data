{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Test.MusicBrainz.CommonTests
    ( testAdd
    , testAliases
    , testAnnotation
    , testCreateFindLatest
    , testEligibleForCleanup
    , testIpiCodes
    , testMerge
    , testResolveReference
    , testResolveRevisionReference
    , testUpdate

    , createAndUpdateSubtree
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.Monoid (mempty)
import           Data.Time (getCurrentTime)

import qualified Data.Set as Set

import           Test.MusicBrainz
import           Test.MusicBrainz.Repository (acid2)

import           MusicBrainz
import           MusicBrainz.Lens
import           MusicBrainz.Data
import           MusicBrainz.Data.Edit
import           MusicBrainz.Data.Editor (register)

--------------------------------------------------------------------------------
createAndUpdateSubtree ::
  (Create a, FindLatest a, Update a, ViewRevision a, Eq b, Show b)
  => (Ref Editor -> MusicBrainz (Tree a))
  -> (Tree a -> Tree a)
  -> (Tree a -> b)
  -> (Ref (Revision a) -> MusicBrainz b)
  -> MusicBrainz ()
createAndUpdateSubtree makeInitialTree modifyTree extractSubtree viewSubtree = do
  editor <- entityRef <$> register acid2

  tree <- makeInitialTree editor

  let creationTree = modifyTree tree
  entity <- autoEdit $ create editor creationTree >>= viewRevision

  preUpdate <- viewSubtree (coreRevision entity)
  liftIO $ preUpdate @?= extractSubtree creationTree

  edit <- createEdit $
    update editor (coreRevision entity) tree

  apply edit

  latest <- findLatest (coreRef entity)
  postUpdate <- viewSubtree (coreRevision latest)
  liftIO $ postUpdate @?= extractSubtree tree


--------------------------------------------------------------------------------
testCreateFindLatest :: (Eq a, Show a, FindLatest a, Create a, ViewRevision a)
  => (Ref Editor -> MusicBrainz (Tree a)) -> MusicBrainz ()
testCreateFindLatest makeTree = do
  editor <- entityRef <$> register acid2
  tree <- makeTree editor
  created <- autoEdit $ create editor tree >>= viewRevision
  found <- findLatest (coreRef created)
  liftIO $ found @?= created


--------------------------------------------------------------------------------
testResolveRevisionReference :: (Create a, ResolveReference (Revision a))
  => (Ref Editor -> MusicBrainz (Tree a)) -> MusicBrainz ()
testResolveRevisionReference makeTree = do
  editor <- entityRef <$> register acid2
  tree <- makeTree editor
  createdRev <- autoEdit $ create editor tree
  found <- resolveReference (dereference createdRev)
  liftIO $ found @?= Just createdRev

  now <- liftIO getCurrentTime
  foundRevision <- traverse getEntity found
  assertBool "Found a revision" $
    fmap (revisionCreatedAt . entityData) foundRevision < Just now


--------------------------------------------------------------------------------
testUpdate :: (Create a, Update a, FindLatest a, ViewRevision a, ViewTree a)
  => Tree a -> Tree a -> MusicBrainz ()
testUpdate start end = do
  editor <- entityRef <$> register acid2

  created <- autoEdit $ create editor start >>= viewRevision
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

  children <- revisionChildren (coreRevision created)
  liftIO $
    coreRevision found `Set.member` children @? "Original entity has revision as a child"


--------------------------------------------------------------------------------
testMerge :: (RefSpec a ~ MBID a, Merge a, Referenceable a, ResolveReference a, Create a, ViewRevision a)
  => (Ref Editor -> MusicBrainz (Tree a, Tree a)) -> MusicBrainz ()
testMerge makeTrees = do
  editor <- entityRef <$> register acid2

  (treeA, treeB) <- makeTrees editor
  a <- autoEdit $ create editor treeA >>= viewRevision
  b <- autoEdit $ create editor treeB >>= viewRevision

  edit <- createEdit $
    merge editor (coreRevision a) (coreRef b)

  apply edit

  aResolved <- resolveReference (dereference $ coreRef a)
  liftIO $ aResolved @?= Just (coreRef b)


--------------------------------------------------------------------------------
testAliases :: (Create a, FindLatest a, TreeAliases a, Update a, ViewAliases a, ViewRevision a)
  => Tree a -> (Alias a) -> MusicBrainz ()
testAliases tree alias = do
  editor <- entityRef <$> register acid2

  artist <- autoEdit $ create editor (aliases .~ Set.singleton alias $ tree) >>= viewRevision
  aliasesPreUpdate <- viewAliases (coreRevision artist)
  liftIO $ aliasesPreUpdate @?= Set.singleton alias

  edit <- createEdit $
    update editor (coreRevision artist) tree

  apply edit

  latest <- findLatest (coreRef artist)
  aliasesPostUpdate <- viewAliases (coreRevision latest)
  liftIO $ aliasesPostUpdate @?= mempty


--------------------------------------------------------------------------------
testAnnotation :: (Create a, FindLatest a, TreeAnnotation a, Update a, ViewAnnotation a, ViewRevision a)
  => (Ref Editor -> MusicBrainz (Tree a)) -> MusicBrainz ()
testAnnotation startTree = do
  editor <- entityRef <$> register acid2

  entityTree <- startTree editor
  entity <- autoEdit $ create editor (annotation .~ expected $ entityTree) >>= viewRevision
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


--------------------------------------------------------------------------------
testAdd :: (Add a, Eq a, Show a)
  => a -> MusicBrainz ()
testAdd e = do
  created <- add e
  liftIO $ entityData created @?= e


--------------------------------------------------------------------------------
testResolveReference :: (Eq a, ResolveReference a, Referenceable a, Show a)
  => MusicBrainz e -> (e -> Ref a) -> MusicBrainz ()
testResolveReference runCreate getRef = do
  expected <- getRef <$> runCreate
  actual <- resolveReference (dereference expected)
  liftIO $ actual @?= Just expected


--------------------------------------------------------------------------------
testIpiCodes :: (Create a, FindLatest a, TreeIPICodes a, Update a, ViewIPICodes a, ViewRevision a)
  => Tree a -> MusicBrainz ()
testIpiCodes startTree = do
  editor <- entityRef <$> register acid2

  entity <- autoEdit $ create editor (ipiCodes .~ Set.singleton expected $ startTree) >>= viewRevision
  ipiPreUpdate <- viewIpiCodes (coreRevision entity)
  liftIO $ ipiPreUpdate @?= Set.singleton expected

  edit <- createEdit $
    update editor (coreRevision entity) startTree

  apply edit

  latest <- findLatest (coreRef entity)
  ipiPostUpdate <- viewIpiCodes (coreRevision latest)
  liftIO $ ipiPostUpdate @?= mempty

  where
    expected = "12345678912" ^?! ipi


--------------------------------------------------------------------------------
testEligibleForCleanup :: (Create a, HoldsRelationships a, ViewRevision a) => (Ref Editor -> MusicBrainz (Tree a)) -> MusicBrainz ()
testEligibleForCleanup startTree = do
  editor <- entityRef <$> register acid2
  fullTree <- startTree editor
  entity <- autoEdit $ create editor fullTree >>= viewRevision
  eligible <- eligibleForCleanup (coreRevision entity)
  liftIO $ eligible @?= False
