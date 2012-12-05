{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Label.Tests ( tests ) where

import Test.MusicBrainz
import Test.MusicBrainz.Data

import MusicBrainz

import qualified MusicBrainz.Data.ClassTests as ClassTests

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAliases
        , testAnnotation
        , testCreateFindLatest
        , testIpiCodes
        , testMerge
        , testUpdate
        ]


--------------------------------------------------------------------------------
testAliases :: Test
testAliases = testCase "Can add and remove aliases" $ mbTest $ do
  ClassTests.testAliases revolutionRecords alias
  where
    alias = Alias { aliasName = "Rev Recs"
                  , aliasSortName = "Recs Rev"
                  , aliasBeginDate = emptyDate
                  , aliasEndDate = emptyDate
                  , aliasEnded = False
                  , aliasType = Nothing
                  , aliasLocale = Nothing
                  }


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
testAnnotation :: Test
testAnnotation = testCase "Can add and remove label annotations" $ mbTest $ do
  ClassTests.testAnnotation (return . const revolutionRecords)


--------------------------------------------------------------------------------
testIpiCodes :: Test
testIpiCodes = testCase "Can add and remove label IPI codes" $ mbTest $ do
  ClassTests.testIpiCodes revolutionRecords


--------------------------------------------------------------------------------
revolutionRecords :: Tree Label
revolutionRecords = minimalTree $
  Label { labelName = "Revolution Records"
        , labelSortName = "Records, Revolution"
        , labelComment = ""
        , labelBeginDate = emptyDate
        , labelEndDate = emptyDate
        , labelEnded = False
        , labelType = Nothing
        , labelCode = Nothing
        }
