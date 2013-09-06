{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.LabelType.Tests ( tests ) where

import Test.MusicBrainz

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz.Label
import MusicBrainz.Versioning

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddLabelType
        , testResolveReference
        ]

--------------------------------------------------------------------------------
testAddLabelType :: Test
testAddLabelType = testCase "Can add LabelTypes" $ do
  CommonTests.testAdd originalProduction


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve LabelTypes" $ do
  CommonTests.testResolveReference (add originalProduction) entityRef


--------------------------------------------------------------------------------
originalProduction :: LabelType
originalProduction = LabelType { labelTypeName = "Original production" }
