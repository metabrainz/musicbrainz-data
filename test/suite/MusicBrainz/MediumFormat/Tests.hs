{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.MediumFormat.Tests ( tests ) where

import Test.MusicBrainz

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz.Class.Add
import MusicBrainz.Entity
import MusicBrainz.Release

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
