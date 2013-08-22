{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Recording.Tests
    ( tests ) where

import Control.Applicative
import Control.Lens
import Data.Monoid (mempty)
import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Data (singleArtistAc)
import Test.MusicBrainz.Repository

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz.Monad
import MusicBrainz.Util (viewOnce)
import MusicBrainz.ArtistCredit
import MusicBrainz.Class.Create
import MusicBrainz.Class.ViewRevision
import MusicBrainz.Editor
import MusicBrainz.Entity
import MusicBrainz.Ref
import MusicBrainz.Recording
import MusicBrainz.Release hiding (findByArtist)
import MusicBrainz.ISRC
import MusicBrainz.Tree

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testCreateFindLatest
        , testAnnotation
        , testIsrc
        , testMerge
        , testResolveRevisionReference
        , testFindRecordingTracks
        , testFindByArtist
        , testFindByIsrc
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
    other <- create editor recTree >>= fmap coreRef . viewRevision
    let t = Track { trackArtistCredit = ac
                   , trackName = "T1"
                   , trackRecording = recording
                   , trackDuration = Nothing
                   , trackPosition = "1"
                   }
        r1_t1 = t { trackName = "Track 1" }
        r1_t2 = t { trackPosition = "2", trackRecording = other }
        r2_t1 = t { trackRecording = other }
        r2_t2 = t { trackPosition = "2" }
        r3_t1 = t { trackRecording = other }

        medium = Medium { mediumName = ""
                        , mediumPosition = 1
                        , mediumFormat = Nothing
                        , mediumCdTocs = mempty
                        , mediumTracks = mempty
                        }

    r1 <- viewRevision =<< create editor
      rtree { releaseMediums = [ medium { mediumTracks = [ r1_t1, r1_t2 ] } ] }

    r2 <- viewRevision =<< create editor
      rtree { releaseMediums = [ medium { mediumTracks = [ r2_t1, r2_t2 ] } ] }

    create editor
      rtree { releaseMediums = [ medium { mediumTracks = [ r3_t1 ] } ] }

    return ( recording
           , [ RecordingUse r1_t1 r1 2
             , RecordingUse r2_t2 r2 2
             ]
           )

  actual <- findRecordingTracks recording
  actual @?= expected


--------------------------------------------------------------------------------
testFindByArtist :: Test
testFindByArtist = testCase "Can find recordings by artist" $ do
  editor <- entityRef <$> register acid2
  recTree <- strangers editor

  expected <- autoEdit $ viewRevision =<< create editor recTree

  artist <- acnArtist . head <$> viewOnce expandCredits
    (recordingArtistCredit . recordingData $ recTree)

  recordings <- findByArtist artist
  recordings @?= [expected]


--------------------------------------------------------------------------------
testFindByIsrc :: Test
testFindByIsrc = testCase "Can find recordings by ISRC" $ do
  editor <- entityRef <$> register acid2
  recTree <- strangers editor

  let i = "GBAAA9800322" ^?! isrc
  expected <- autoEdit $ viewRevision =<< create editor recTree
    { recordingIsrcs = Set.singleton i }

  recordings <- findByIsrc i
  recordings @?= [expected]
