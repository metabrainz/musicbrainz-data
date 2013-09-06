{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.ReleasePackaging.Tests ( tests ) where

import Test.MusicBrainz

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz.Release
import MusicBrainz.Versioning

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddReleasePackaging
        , testResolveReference
        ]

--------------------------------------------------------------------------------
testAddReleasePackaging :: Test
testAddReleasePackaging = testCase "Can add ReleasePackaging" $ do
  CommonTests.testAdd jewelCase


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve ReleasePackaging" $ do
  CommonTests.testResolveReference (add jewelCase) entityRef


--------------------------------------------------------------------------------
jewelCase :: ReleasePackaging
jewelCase = ReleasePackaging { releasePackagingName = "Jewel Case" }
