{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Release.Tests
    ( tests ) where

import Control.Applicative
import Control.Lens

import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Data
import Test.MusicBrainz.Repository (portishead, dummy, uk, acid2, latin, english, revolutionRecords, mysterons, minimalTree)

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz
import MusicBrainz.Data
import MusicBrainz.Data.Editor (register)
import MusicBrainz.Data.Release

import qualified MusicBrainz.Data.ReleasePackaging as ReleasePackaging
import qualified MusicBrainz.Data.ReleaseStatus as ReleaseStatus

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
  CommonTests.testCreateFindLatest dummyTree


--------------------------------------------------------------------------------
testAnnotation :: Test
testAnnotation = testCase "Can add and remove artist annotations" $ do
  CommonTests.testAnnotation dummyTree


--------------------------------------------------------------------------------
testReleaseLabels :: Test
testReleaseLabels = testCase "Releases can have release labels" $ do
  editor <- entityRef <$> register acid2
  revRecLabel <- coreRef <$> autoEdit (create editor revolutionRecords >>= viewRevision)
  let revRec = ReleaseLabel { releaseLabel = Just revRecLabel, releaseCatalogNumber = Just "REVREC001" }

  CommonTests.createAndUpdateSubtree
    dummyTree
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
  tree <- (\t -> t { releaseMediums = mediums }) <$> dummyTree editor

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
      a <- dummyTree editor
      return (a, modTree a)
    modTree t = t { releaseData = (releaseData t) { releaseName = "Blue Lines" } }


--------------------------------------------------------------------------------
testResolveRevisionReference :: Test
testResolveRevisionReference = testCase "Resolve revision reference" $ do
  CommonTests.testResolveRevisionReference dummyTree


--------------------------------------------------------------------------------
dummyTree :: Ref Editor -> MusicBrainz (Tree Release)
dummyTree editor = do
  portisheadAc <- singleArtistAc editor portishead
  portisheadRg <- autoEdit $ create editor (minimalTree (dummy portisheadAc)) >>= viewRevision
  country <- add uk
  script <- add latin
  language <- add english
  packaging <- ReleasePackaging.addReleasePackaging ReleasePackaging
    { releasePackagingName = "Jewel Case" }
  status <- ReleaseStatus.addReleaseStatus ReleaseStatus
    { releaseStatusName = "Official" }
  return $ minimalTree $
    expected (coreRef portisheadRg) portisheadAc
      (entityRef country) (entityRef script) (entityRef language)
      (entityRef packaging) (entityRef status)
  where
    expected rg ac country script language packaging status =
      Release { releaseName = "Dummy"
              , releaseComment = ""
              , releaseArtistCredit = ac
              , releaseReleaseGroup = rg
              , releaseDate = PartialDate (Just 1997) (Just 9) (Just 29)
              , releaseCountry = Just country
              , releaseScript = Just script
              , releaseLanguage = Just language
              , releasePackaging = Just packaging
              , releaseStatus = Just status
              }
