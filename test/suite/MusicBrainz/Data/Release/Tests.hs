{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Release.Tests
    ( tests ) where

import Control.Applicative
import Control.Lens
import Data.Monoid

import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Data
import Test.MusicBrainz.Repository (portishead, dummy, uk, acid2, latin, english, revolutionRecords, mysterons)

import qualified MusicBrainz.Data.ClassTests as ClassTests

import MusicBrainz
import MusicBrainz.Data
import MusicBrainz.Data.Edit
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
        ]


--------------------------------------------------------------------------------
testCreateFindLatest :: Test
testCreateFindLatest = testCase "findLatest when release exists" $ mbTest $ do
  editor <- entityRef <$> register acid2
  created <- dummyTree editor >>= \t -> autoEdit (create editor t >>= viewRevision)

  found <- findLatest (coreRef created)
  liftIO $ found @?= created


--------------------------------------------------------------------------------
testAnnotation :: Test
testAnnotation = testCase "Can add and remove artist annotations" $ mbTest $ do
  ClassTests.testAnnotation dummyTree


--------------------------------------------------------------------------------
testReleaseLabels :: Test
testReleaseLabels = testCase "Releases can have release labels" $ mbTest $ do
  editor <- entityRef <$> register acid2

  tree <- dummyTree editor
  revRecLabel <- coreRef <$> autoEdit (create editor revolutionRecords >>= viewRevision)
  let revRec = ReleaseLabel { releaseLabel = Just revRecLabel, releaseCatalogNumber = Just "REVREC001" }

  release <- autoEdit $ create editor (releaseLabelsLens .~ Set.singleton revRec $ tree) >>= viewRevision
  releaseLabelsPreUpdate <- viewReleaseLabels (coreRevision release)
  liftIO $ releaseLabelsPreUpdate @?= Set.singleton revRec

  edit <- createEdit $
    update editor (coreRevision release) tree

  apply edit

  latest <- findLatest (coreRef release)
  releaseLabelsPostUpdate <- viewReleaseLabels (coreRevision latest)
  liftIO $ releaseLabelsPostUpdate @?= mempty

  where releaseLabelsLens f t = f (releaseLabels t) <&> \b -> t { releaseLabels = b }


--------------------------------------------------------------------------------
testTrackLists :: Test
testTrackLists = testCase "Releases can have track lists" $ mbTest $ do
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
testMerge = testCase "Can merge 2 distinct releases" $ mbTest $ do
  ClassTests.testMerge createRecordings
  where
    createRecordings editor = do
      a <- dummyTree editor
      return (a, modTree a)
    modTree t = t { releaseData = (releaseData t) { releaseName = "Blue Lines" } }


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
