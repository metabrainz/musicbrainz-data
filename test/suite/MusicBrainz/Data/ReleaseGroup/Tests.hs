{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.ReleaseGroup.Tests
    ( tests ) where

import Data.Maybe (fromJust)
import Test.MusicBrainz

import MusicBrainz
import MusicBrainz.Data.ReleaseGroup ()
import MusicBrainz.Data.FindLatest

tests :: [Test]
tests = [ testFindLatest
        ]

testFindLatest :: Test
testFindLatest = testCase "findLatest when release group exists" $
  mbTest (findLatest knownReleaseGroupId) >>= (@?= expected)
  where
    knownReleaseGroupId = fromJust $ parseMbid "d7ec6175-0891-448d-b9e4-14d007d53d29"
    expected = Just
      CoreEntity { coreMbid = knownReleaseGroupId
                 , coreRevision = RevisionRef 10761
                 , coreData =
                     ReleaseGroup { releaseGroupName = "Portishead"
                                  , releaseGroupComment = ""
                                  , releaseGroupPrimaryType =
                                      Just (ReleaseGroupTypeRef 1)
                                  , releaseGroupArtistCredit = ArtistCreditRef 1
                                  }
                 }
