{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.LabelType.Tests ( tests ) where

import Test.MusicBrainz

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz
import MusicBrainz.Data

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddLabelType
        , testResolveReference
        ]

--------------------------------------------------------------------------------
testAddLabelType :: Test
testAddLabelType = testCase "Can add LabelTypes" $ mbTest $ do
  CommonTests.testAdd originalProduction


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve LabelTypes" $ mbTest $ do
  CommonTests.testResolveReference (add originalProduction) entityRef


--------------------------------------------------------------------------------
originalProduction :: LabelType
originalProduction = LabelType { labelTypeName = "Original production" }
