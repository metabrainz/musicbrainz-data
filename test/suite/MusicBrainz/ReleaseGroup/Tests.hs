{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.ReleaseGroup.Tests
    ( tests ) where

import Control.Applicative

import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Data
import Test.MusicBrainz.Repository

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz.ArtistCredit
import MusicBrainz.EditApplication
import MusicBrainz.ReleaseGroup
import MusicBrainz.Util (viewOnce)
import MusicBrainz.Versioning

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testCreateFindLatest
        , testAnnotation
        , testSecondaryTypes
        , testMerge
        , testResolveRevisionReference
        , testFindByArtist
        ]


--------------------------------------------------------------------------------
testCreateFindLatest :: Test
testCreateFindLatest = testCase "create/findLatest" $
  CommonTests.testCreateFindLatest dummyReleaseGroupTree


--------------------------------------------------------------------------------
testAnnotation :: Test
testAnnotation = testCase "Can add and remove artist annotations" $ do
  CommonTests.testAnnotation dummyReleaseGroupTree


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
    treeA <- dummyReleaseGroupTree editor
    let treeB = minimalTree $ (treeData treeA) { releaseGroupName = "Lungbone" }
    return (treeA, treeB)


--------------------------------------------------------------------------------
testResolveRevisionReference :: Test
testResolveRevisionReference = testCase "Resolve revision reference" $ do
  CommonTests.testResolveRevisionReference dummyReleaseGroupTree


--------------------------------------------------------------------------------
testFindByArtist :: Test
testFindByArtist = testCase "Can find release groups by artist" $ do
  editor <- entityRef <$> register acid2

  edit <- openEdit
  ac <- singleArtistAc editor portishead
  comp <- add compilation
  expected <- withEdit edit $
    viewRevision =<< (create editor $ minimalTree (dummy ac)
                        { releaseGroupSecondaryTypes = Set.singleton (entityRef comp) })

  apply edit

  [acn] <- viewOnce expandCredits ac

  actual <- findByArtist (acnArtist acn)
  actual @?= [expected]
