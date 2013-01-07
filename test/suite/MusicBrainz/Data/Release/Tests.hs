{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Release.Tests
    ( tests ) where

import Control.Applicative
import Control.Lens

import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Repository (dummyReleaseTree, acid2, revolutionRecords, mysterons)

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz
import MusicBrainz.Data
import MusicBrainz.Data.Editor (register)
import MusicBrainz.Data.Release

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testCreateFindLatest
        , testAnnotation
        , testReleaseLabels
        , testTrackLists
        , testMerge
        , testResolveRevisionReference
        ]


--------------------------------------------------------------------------------
testCreateFindLatest :: Test
testCreateFindLatest = testCase "findLatest when release exists" $
  CommonTests.testCreateFindLatest dummyReleaseTree


--------------------------------------------------------------------------------
testAnnotation :: Test
testAnnotation = testCase "Can add and remove artist annotations" $ do
  CommonTests.testAnnotation dummyReleaseTree


--------------------------------------------------------------------------------
testReleaseLabels :: Test
testReleaseLabels = testCase "Releases can have release labels" $ do
  editor <- entityRef <$> register acid2
  revRecLabel <- coreRef <$> autoEdit (create editor revolutionRecords >>= viewRevision)
  let revRec = ReleaseLabel { releaseLabel = Just revRecLabel, releaseCatalogNumber = Just "REVREC001" }

  CommonTests.createAndUpdateSubtree
    dummyReleaseTree
    (releaseLabelsLens .~ Set.singleton revRec)
    releaseLabels
    viewReleaseLabels

  where
    releaseLabelsLens f t = f (releaseLabels t) <&> \b -> t { releaseLabels = b }


--------------------------------------------------------------------------------
testTrackLists :: Test
testTrackLists = testCase "Releases can have track lists" $ do
  editor <- entityRef <$> register acid2

  mediums <- makeMediums editor
  tree <- (\t -> t { releaseMediums = mediums }) <$> dummyReleaseTree editor

  release <- autoEdit $ create editor tree >>= viewRevision
  createdMediums <- viewMediums (coreRevision release)

  liftIO $ createdMediums @?= mediums

  where
    makeMediums editor = do
      t <- mysterons editor
      mystRec <- autoEdit $ create editor t >>= viewRevision
      return $ [ Medium { mediumName = "Live"
                        , mediumFormat = Nothing
                        , mediumPosition = 1
                        , mediumTracks = [ Track { trackName = "Mysterons"
                                                 , trackRecording = coreRef mystRec
                                                 , trackDuration = Nothing
                                                 , trackArtistCredit = (recordingArtistCredit (coreData mystRec))
                                                 , trackPosition = "1"
                                                 } ] } ]


--------------------------------------------------------------------------------
testMerge :: Test
testMerge = testCase "Can merge 2 distinct releases" $ do
  CommonTests.testMerge createRecordings
  where
    createRecordings editor = do
      a <- dummyReleaseTree editor
      return (a, modTree a)
    modTree t = t { releaseData = (releaseData t) { releaseName = "Blue Lines" } }


--------------------------------------------------------------------------------
testResolveRevisionReference :: Test
testResolveRevisionReference = testCase "Resolve revision reference" $ do
  CommonTests.testResolveRevisionReference dummyReleaseTree
