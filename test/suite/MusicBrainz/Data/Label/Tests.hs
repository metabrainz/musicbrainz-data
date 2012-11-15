{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Label.Tests ( tests ) where

import Test.MusicBrainz

import MusicBrainz

import qualified MusicBrainz.Data.ClassTests as ClassTests

tests :: [Test]
tests = [ testCreateFindLatest
        ]

testCreateFindLatest :: Test
testCreateFindLatest = testCase "create >>= findLatest == create" $ mbTest $ do
  ClassTests.testCreateFindLatest (LabelTree expected)
  where
    expected = Label { labelName = "Revolution Records"
                     , labelSortName = "Records, Revolution"
                     , labelComment = ""
                     , labelBeginDate = emptyDate
                     , labelEndDate = emptyDate
                     , labelEnded = False
                     , labelType = Nothing
                     , labelCode = Nothing
                     }
