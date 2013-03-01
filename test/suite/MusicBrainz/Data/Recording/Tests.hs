{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Recording.Tests
    ( tests ) where

import Control.Applicative
import Control.Lens
import Data.Maybe (fromJust)
import Data.Monoid (mempty)
import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Data (singleArtistAc)
import Test.MusicBrainz.Repository

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz
import MusicBrainz.Data
import MusicBrainz.Data.Editor
import MusicBrainz.Data.Recording

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testCreateFindLatest
        , testAnnotation
        , testIsrc
        , testPuid
        , testMerge
        , testResolveRevisionReference
        , testFindRecordingTracks
        ]


--------------------------------------------------------------------------------
testCreateFindLatest :: Test
testCreateFindLatest = testCase "findLatest when recording exists" $
  CommonTests.testCreateFindLatest mysterons


--------------------------------------------------------------------------------
testAnnotation :: Test
testAnnotation = testCase "Can add and remove recording annotations" $ do
  CommonTests.testAnnotation mysterons


--------------------------------------------------------------------------------
testIsrc :: Test
testIsrc = testCase "Can add and remove ISRCs" $
  CommonTests.createAndUpdateSubtree
    mysterons
    withIsrc
    recordingIsrcs
    viewIsrcs

  where
    expected = "GBAAA9800322" ^?! isrc
    withIsrc t = t { recordingIsrcs = Set.singleton expected }


--------------------------------------------------------------------------------
testPuid :: Test
testPuid = testCase "Can add and remove PUIDs" $
  CommonTests.createAndUpdateSubtree
    mysterons
    withPuid
    recordingPuids
    viewPuids

  where
    expected = fromJust ("3893a0d0-3fd1-11e2-a25f-0800200c9a66" ^? puid)
    withPuid t = t { recordingPuids = Set.singleton expected }


--------------------------------------------------------------------------------
testMerge :: Test
testMerge = testCase "Can merge 2 distinct recordings" $ do
  CommonTests.testMerge createRecordings
  where
    createRecordings editor = do
      a <- mysterons editor
      b <- strangers editor
      return (a, b)


--------------------------------------------------------------------------------
strangers :: Ref Editor -> MusicBrainz (Tree Recording)
strangers editor = do
  ac <- singleArtistAc editor portishead
  return $ minimalTree $
    Recording { recordingName = "Strangers"
              , recordingComment = ""
              , recordingArtistCredit = ac
              , recordingDuration = Nothing
              }


--------------------------------------------------------------------------------
testResolveRevisionReference :: Test
testResolveRevisionReference = testCase "Resolve revision reference" $ do
  CommonTests.testResolveRevisionReference mysterons


--------------------------------------------------------------------------------
testFindRecordingTracks :: Test
testFindRecordingTracks = testCase "Can find tracks for a given recording" $ do
  editor <- entityRef <$> register acid2
  recTree <- strangers editor
  rtree <- dummyReleaseTree editor
  ac <- singleArtistAc editor portishead

  (recording, expected) <- autoEdit $ do
    recording <- create editor recTree >>= fmap coreRef . viewRevision
    let t1 = Track { trackArtistCredit = ac
                   , trackName = "T1"
                   , trackRecording = recording
                   , trackDuration = Nothing
                   , trackPosition = "1"
                   }
    let t2 = t1 { trackName = "Track 1" }
    let medium = Medium { mediumName = ""
                        , mediumPosition = 1
                        , mediumFormat = Nothing
                        , mediumCdTocs = mempty
                        , mediumTracks = mempty
                        }

    r1 <- viewRevision =<< create editor
      rtree { releaseMediums = [ medium { mediumTracks = [ t1 ] } ] }

    r2 <- viewRevision =<< create editor
      rtree { releaseMediums = [ medium { mediumTracks = [ t2 ] } ] }

    return ( recording
           , [ RecordingUse t1 r1
             , RecordingUse t2 r2
             ]
           )

  actual <- findRecordingTracks recording
  actual @?= expected
