{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.MediumFormat.Tests ( tests ) where

import Test.MusicBrainz

import MusicBrainz
import MusicBrainz.Data

import qualified MusicBrainz.Data.ClassTests as ClassTests

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddMediumFormat
        , testResolveReference
        ]

--------------------------------------------------------------------------------
testAddMediumFormat :: Test
testAddMediumFormat = testCase "Can add MediumFormats" $ mbTest $ do
  ClassTests.testAdd digitalMedia


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve MediumFormats" $ mbTest $ do
  ClassTests.testResolveReference (add digitalMedia) entityRef


--------------------------------------------------------------------------------
digitalMedia :: MediumFormat
digitalMedia = MediumFormat { mediumFormatName = "Digital Media" }
