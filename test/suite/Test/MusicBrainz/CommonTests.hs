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
    , testIsniCodes
    , testMerge
    , testResolveReference
    , testResolveRevisionReference
    , testUpdate

    , createAndUpdateSubtree
    ) where

import Control.Applicative
import Control.Lens hiding (children)
import Data.Monoid (mempty)
import Data.Time (getCurrentTime)

import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Repository (acid2)

import MusicBrainz.Util
import MusicBrainz.Monad
import MusicBrainz.Alias
import MusicBrainz.Annotation
import MusicBrainz.Class.Add
import MusicBrainz.Class.Cleanup
import MusicBrainz.Class.Create
import MusicBrainz.Class.FindLatest
import MusicBrainz.Class.GetEntity
import MusicBrainz.Class.ResolveReference
import MusicBrainz.Class.Update
import MusicBrainz.Class.ViewRevision
import MusicBrainz.Edit
import MusicBrainz.EditApplication
import MusicBrainz.Editor
import MusicBrainz.Entity
import MusicBrainz.IPI
import MusicBrainz.ISNI
import MusicBrainz.MBID
import MusicBrainz.Ref
import MusicBrainz.Relationship.Internal
import MusicBrainz.Revision
import MusicBrainz.Revision.Internal
import MusicBrainz.Tree

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

  latest <- viewOnce findLatest (coreRef entity)
  postUpdate <- viewSubtree (coreRevision latest)
  liftIO $ postUpdate @?= extractSubtree tree


--------------------------------------------------------------------------------
testCreateFindLatest :: (Eq a, Show a, FindLatest a, Create a, ViewRevision a)
  => (Ref Editor -> MusicBrainz (Tree a)) -> MusicBrainz ()
testCreateFindLatest makeTree = do
  editor <- entityRef <$> register acid2
  tree <- makeTree editor
  created <- autoEdit $ create editor tree >>= viewRevision
  found <- viewOnce findLatest (coreRef created)
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
testUpdate :: (Create a, Update a, FindLatest a, ViewRevision a, ViewTree a, Show (Tree a), Eq (Tree a))
  => Tree a -> Tree a -> MusicBrainz ()
testUpdate start end = do
  editor <- entityRef <$> register acid2

  created <- autoEdit $ create editor start >>= viewRevision
  let entityId = coreRef created

  editId <- createEdit $
    update editor (coreRevision created) end

  apply editId

  found <- viewOnce findLatest entityId
  latestTree <- viewTree (coreRevision found)
  liftIO $ latestTree @?= end

  parents <- revisionParents (coreRevision found)
  liftIO $
    coreRevision created `Set.member` parents @? "Is parented to starting revision"

  children <- revisionChildren (coreRevision created)
  liftIO $
    coreRevision found `Set.member` children @? "Original entity has revision as a child"


--------------------------------------------------------------------------------
testMerge :: (RefSpec a ~ MBID a, Referenceable a, ResolveReference a, Create a, ViewRevision a, CloneRevision a, Editable a, FindLatest a)
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

  latest <- viewOnce findLatest (coreRef artist)
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

  latest <- viewOnce findLatest (coreRef entity)
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

  entity <- autoEdit $ create editor (ipiCodes .~ expected $ startTree) >>= viewRevision
  ipiPreUpdate <- viewOnce viewIpiCodes (coreRevision entity)
  liftIO $ ipiPreUpdate @?= expected

  edit <- createEdit $
    update editor (coreRevision entity) startTree

  apply edit

  latest <- viewOnce findLatest (coreRef entity)
  ipiPostUpdate <- viewOnce viewIpiCodes (coreRevision latest)
  liftIO $ ipiPostUpdate @?= mempty

  where
    expected = Set.singleton $ "12345678912" ^?! ipi


--------------------------------------------------------------------------------
testIsniCodes :: (Create a, FindLatest a, TreeISNICodes a, Update a, ViewISNICodes a, ViewRevision a)
  => Tree a -> MusicBrainz ()
testIsniCodes startTree = do
  editor <- entityRef <$> register acid2

  entity <- autoEdit $ create editor (isniCodes .~ expected $ startTree) >>= viewRevision
  isniPreUpdate <- viewOnce viewIsniCodes (coreRevision entity)
  liftIO $ isniPreUpdate @?= expected

  edit <- createEdit $
    update editor (coreRevision entity) startTree

  apply edit

  latest <- viewOnce findLatest (coreRef entity)
  isniPostUpdate <- viewOnce viewIsniCodes (coreRevision latest)
  liftIO $ isniPostUpdate @?= mempty

  where
    expected = Set.singleton $ "1234567898765432" ^?! isni


--------------------------------------------------------------------------------
testEligibleForCleanup :: (Create a, HoldsRelationships a, ViewRevision a) => (Ref Editor -> MusicBrainz (Tree a)) -> MusicBrainz ()
testEligibleForCleanup startTree = do
  editor <- entityRef <$> register acid2
  fullTree <- startTree editor
  entity <- autoEdit $ create editor fullTree >>= viewRevision
  eligible <- eligibleForCleanup (coreRevision entity)
  liftIO $ eligible @?= False
