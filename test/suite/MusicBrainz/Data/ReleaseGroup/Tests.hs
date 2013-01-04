{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.ReleaseGroup.Tests
    ( tests ) where

import Control.Applicative

import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Data
import Test.MusicBrainz.Repository (portishead, dummy, compilation, minimalTree)

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz
import MusicBrainz.Data

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testCreateFindLatest
        , testAnnotation
        , testSecondaryTypes
        , testMerge
        , testResolveRevisionReference
        ]


--------------------------------------------------------------------------------
testCreateFindLatest :: Test
testCreateFindLatest = testCase "create/findLatest" $
  CommonTests.testCreateFindLatest dummyTree


--------------------------------------------------------------------------------
testAnnotation :: Test
testAnnotation = testCase "Can add and remove artist annotations" $ do
  CommonTests.testAnnotation dummyTree


--------------------------------------------------------------------------------
testSecondaryTypes :: Test
testSecondaryTypes = testCase "Release groups can have secondary types" $ do
  types <- Set.fromList . map entityRef <$> sequence [ add compilation, add remix ]

  CommonTests.createAndUpdateSubtree
    makeTree
    (withTypes types)
    (releaseGroupSecondaryTypes . treeData)
    (fmap (releaseGroupSecondaryTypes . coreData) . viewRevision)

  where
    remix = ReleaseGroupType { releaseGroupTypeName = "Remix" }

    makeTree editor = do
      ac <- singleArtistAc editor portishead
      return $ minimalTree (dummy ac)

    withTypes types t = t {
        releaseGroupData = (releaseGroupData t)
          { releaseGroupSecondaryTypes = types }
      }


--------------------------------------------------------------------------------
testMerge :: Test
testMerge = testCase "Can merge two release groups" $ do
  CommonTests.testMerge $ \editor -> do
    treeA <- dummyTree editor
    let treeB = minimalTree $ (treeData treeA) { releaseGroupName = "Lungbone" }
    return (treeA, treeB)


--------------------------------------------------------------------------------
testResolveRevisionReference :: Test
testResolveRevisionReference = testCase "Resolve revision reference" $ do
  CommonTests.testResolveRevisionReference dummyTree


--------------------------------------------------------------------------------
dummyTree :: Ref Editor -> MusicBrainz (Tree ReleaseGroup)
dummyTree editor = do
  ac <- singleArtistAc editor portishead
  return $ minimalTree (dummy ac)
