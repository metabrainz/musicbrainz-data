{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.ReleaseGroup.Tests
    ( tests ) where

import Control.Applicative

import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Data
import Test.MusicBrainz.Repository (portishead, dummy, acid2, compilation)

import qualified MusicBrainz.Data.ClassTests as ClassTests

import MusicBrainz
import MusicBrainz.Data
import MusicBrainz.Data.Editor

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testFindLatest
        , testAnnotation
        , testSecondaryTypes
        , testMerge
        , testResolveRevisionReference
        ]


--------------------------------------------------------------------------------
testFindLatest :: Test
testFindLatest = testCase "findLatest when release group exists" $ mbTest $ do
  editor <- entityRef <$> register acid2
  created <- dummyTree editor >>= \t -> autoEdit (create editor t >>= viewRevision)
  found <- findLatest (coreRef created)
  liftIO $ found @?= created


--------------------------------------------------------------------------------
testAnnotation :: Test
testAnnotation = testCase "Can add and remove artist annotations" $ mbTest $ do
  ClassTests.testAnnotation dummyTree


--------------------------------------------------------------------------------
testSecondaryTypes :: Test
testSecondaryTypes = testCase "Release groups can have secondary types" $ mbTest $ do
  editor <- entityRef <$> register acid2

  types <- Set.fromList . map entityRef <$> sequence [ add compilation, add remix ]
  tree <- do
    ac <- singleArtistAc editor portishead
    return $ minimalTree (dummy ac) { releaseGroupSecondaryTypes = types }

  created <- autoEdit $ create editor tree >>= viewRevision
  liftIO $ releaseGroupSecondaryTypes (coreData created) @?= types
  where remix = ReleaseGroupType { releaseGroupTypeName = "Remix" }


--------------------------------------------------------------------------------
testMerge :: Test
testMerge = testCase "Can merge two release groups" $ mbTest $ do
  ClassTests.testMerge $ \editor -> do
    treeA <- dummyTree editor
    let treeB = minimalTree $ (treeData treeA) { releaseGroupName = "Lungbone" }
    return (treeA, treeB)


--------------------------------------------------------------------------------
testResolveRevisionReference :: Test
testResolveRevisionReference = testCase "Resolve revision reference" $ mbTest $ do
  ClassTests.testResolveRevisionReference dummyTree


--------------------------------------------------------------------------------
dummyTree :: Ref Editor -> MusicBrainz (Tree ReleaseGroup)
dummyTree editor = do
  ac <- singleArtistAc editor portishead
  return $ minimalTree (dummy ac)
