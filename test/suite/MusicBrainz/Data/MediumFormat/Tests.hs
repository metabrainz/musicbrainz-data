{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.MediumFormat.Tests ( tests ) where

import Test.MusicBrainz

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz
import MusicBrainz.Data

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddMediumFormat
        , testResolveReference
        ]

--------------------------------------------------------------------------------
testAddMediumFormat :: Test
testAddMediumFormat = testCase "Can add MediumFormats" $ do
  CommonTests.testAdd digitalMedia


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve MediumFormats" $ do
  CommonTests.testResolveReference (add digitalMedia) entityRef


--------------------------------------------------------------------------------
digitalMedia :: MediumFormat
digitalMedia = MediumFormat { mediumFormatName = "Digital Media" }
