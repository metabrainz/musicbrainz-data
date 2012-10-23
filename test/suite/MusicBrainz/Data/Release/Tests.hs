{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Release.Tests
    ( tests ) where

import Data.Maybe (fromJust)
import Test.MusicBrainz

import MusicBrainz
import MusicBrainz.Data.Release ()
import MusicBrainz.Data.FindLatest

tests :: [Test]
tests = [ testFindLatest
        ]

testFindLatest :: Test
testFindLatest = testCase "findLatest when release exists" $
  mbTest (findLatest knownReleaseId) >>= (@?= expected)
  where
    knownMbid = fromJust . parseMbid
    knownReleaseId = knownMbid "df907840-9e5a-41cd-a44d-f039cdecdca4"
    expected = Just
      CoreEntity { coreMbid = knownReleaseId
                 , coreRevision = RevisionRef 10762
                 , coreData =
                     Release { releaseName = "Portishead"
                             , releaseComment = ""
                             , releaseArtistCredit = ArtistCreditRef 1
                             , releaseReleaseGroup =ReleaseGroupRef $ knownMbid
                                 "d7ec6175-0891-448d-b9e4-14d007d53d29"
                             , releaseDate = PartialDate
                                 (Just 1997) (Just 9) (Just 29)
                             , releaseCountry = Just (CountryRef 1)
                             , releaseScript = Just (ScriptRef 3)
                             , releaseLanguage = Just (LanguageRef 1)
                             , releasePackaging = Just (ReleasePackagingRef 1)
                             , releaseStatus = Just (ReleaseStatusRef 1)
                             }
                 }
