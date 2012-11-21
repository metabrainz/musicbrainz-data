{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Label.Tests ( tests ) where

import qualified Data.Set as Set

import Test.MusicBrainz

import MusicBrainz

import qualified MusicBrainz.Data.ClassTests as ClassTests

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testCreateFindLatest
        , testMerge
        , testUpdate
        ]


--------------------------------------------------------------------------------
testCreateFindLatest :: Test
testCreateFindLatest = testCase "create >>= findLatest == create" $ mbTest $ do
  ClassTests.testCreateFindLatest revolutionRecords


--------------------------------------------------------------------------------
testUpdate :: Test
testUpdate = testCase "update does change labels" $ mbTest $ do
  ClassTests.testUpdate revolutionRecords updated
  where
    updated = revolutionRecords { labelData = changedData }
    changedData = (labelData revolutionRecords) { labelName = "Updated Name"
                                                , labelSortName = "Updated!!!"
                                                }


--------------------------------------------------------------------------------
testMerge :: Test
testMerge = testCase "can merge two labels" $ mbTest $ do
  ClassTests.testMerge a b
  where
    a = revolutionRecords
    b = revolutionRecords { labelData = changedData }
    changedData = (labelData revolutionRecords) { labelName = "Updated Name"
                                                , labelSortName = "Updated!!!"
                                                }


--------------------------------------------------------------------------------
revolutionRecords :: Tree Label
revolutionRecords = LabelTree ldat Set.empty Set.empty ""
  where ldat = Label { labelName = "Revolution Records"
                     , labelSortName = "Records, Revolution"
                     , labelComment = ""
                     , labelBeginDate = emptyDate
                     , labelEndDate = emptyDate
                     , labelEnded = False
                     , labelType = Nothing
                     , labelCode = Nothing
                     }
