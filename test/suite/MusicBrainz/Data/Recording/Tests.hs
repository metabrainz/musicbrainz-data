{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Recording.Tests
    ( tests ) where

import Data.Maybe (fromJust)
import Test.MusicBrainz

import MusicBrainz
import MusicBrainz.Data.Recording ()
import MusicBrainz.Data.FindLatest

tests :: [Test]
tests = [ testFindLatest
        ]

testFindLatest :: Test
testFindLatest = testCase "findLatest when recording exists" $
  mbTest (findLatest knownRecordingId) >>= (@?= expected)
  where
    knownRecordingId = fromJust $ parseMbid "c3c2ed6e-7944-4ed5-b597-3d15dc1718dd"
    expected = Just
      CoreEntity { coreMbid = knownRecordingId
                 , coreRevision = RevisionRef 10751
                 , coreData =
                     Recording { recordingName = "I Love Acid"
                               , recordingComment = ""
                               , recordingArtistCredit = ArtistCreditRef 1
                               , recordingDuration = 64936
                               }
                 }
