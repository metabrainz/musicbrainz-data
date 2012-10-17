{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Label.Tests ( tests ) where

import Data.Maybe (fromJust)
import Test.MusicBrainz

import MusicBrainz
import MusicBrainz.Data.Label ()
import MusicBrainz.Data.FindLatest

tests :: [Test]
tests = [ testFindLatest
        ]

testFindLatest :: Test
testFindLatest = testCase "findLatest when label exists" $
  mbTest (findLatest knownLabelId) >>= (@?= expected)
  where
    knownLabelId = fromJust $ parseMbid "7de490ac-84e4-4eab-875d-670fee081968"
    expected = Just
      CoreEntity { coreMbid = knownLabelId
                 , coreRevision = RevisionRef 30
                 , coreData =
                     Label { labelName = "Revolution Records"
                           , labelSortName = "Records, Revolution"
                           , labelComment = ""
                           , labelBeginDate = emptyDate
                           , labelEndDate = emptyDate
                           , labelEnded = False
                           , labelType = Nothing
                           , labelCode = Nothing
                           }
                 }
