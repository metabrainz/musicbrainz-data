{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.LabelType.Tests ( tests ) where

import Test.MusicBrainz

import MusicBrainz
import MusicBrainz.Data

import qualified MusicBrainz.Data.ClassTests as ClassTests

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddLabelType
        , testResolveReference
        ]

--------------------------------------------------------------------------------
testAddLabelType :: Test
testAddLabelType = testCase "Can add LabelTypes" $ mbTest $ do
  ClassTests.testAdd originalProduction


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve LabelTypes" $ mbTest $ do
  ClassTests.testResolveReference (add originalProduction) entityRef


--------------------------------------------------------------------------------
originalProduction :: LabelType
originalProduction = LabelType { labelTypeName = "Original production" }
