{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Label.Tests ( tests ) where

import Test.MusicBrainz
import Test.MusicBrainz.Repository (revolutionRecords)

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAliases
        , testAnnotation
        , testCreateFindLatest
        , testIpiCodes
        , testMerge
        , testUpdate
        , testResolveRevisionReference
        , testIsniCodes
        ]


--------------------------------------------------------------------------------
testAliases :: Test
testAliases = testCase "Can add and remove aliases" $ do
  CommonTests.testAliases revolutionRecords alias
  where
    alias = Alias { aliasName = "Rev Recs"
                  , aliasSortName = "Recs Rev"
                  , aliasBeginDate = emptyDate
                  , aliasEndDate = emptyDate
                  , aliasEnded = False
                  , aliasType = Nothing
                  , aliasLocale = Nothing
                  , aliasPrimaryForLocale = True
                  }


--------------------------------------------------------------------------------
testCreateFindLatest :: Test
testCreateFindLatest = testCase "create >>= findLatest == create" $ do
  CommonTests.testCreateFindLatest (return . const revolutionRecords)


--------------------------------------------------------------------------------
testUpdate :: Test
testUpdate = testCase "update does change labels" $ do
  CommonTests.testUpdate revolutionRecords updated
  where
    updated = revolutionRecords { labelData = changedData }
    changedData = (labelData revolutionRecords) { labelName = "Updated Name"
                                                , labelSortName = "Updated!!!"
                                                }


--------------------------------------------------------------------------------
testMerge :: Test
testMerge = testCase "can merge two labels" $ do
  CommonTests.testMerge (return . const (a, b))
  where
    a = revolutionRecords
    b = revolutionRecords { labelData = changedData }
    changedData = (labelData revolutionRecords) { labelName = "Updated Name"
                                                , labelSortName = "Updated!!!"
                                                }


--------------------------------------------------------------------------------
testAnnotation :: Test
testAnnotation = testCase "Can add and remove label annotations" $ do
  CommonTests.testAnnotation (return . const revolutionRecords)


--------------------------------------------------------------------------------
testIpiCodes :: Test
testIpiCodes = testCase "Can add and remove label IPI codes" $ do
  CommonTests.testIpiCodes revolutionRecords


--------------------------------------------------------------------------------
testIsniCodes :: Test
testIsniCodes = testCase "Can add and remove label ISNI codes" $ do
  CommonTests.testIsniCodes revolutionRecords


--------------------------------------------------------------------------------
testResolveRevisionReference :: Test
testResolveRevisionReference = testCase "Resolve revision reference" $ do
  CommonTests.testResolveRevisionReference (return . const revolutionRecords)
