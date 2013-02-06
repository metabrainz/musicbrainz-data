{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.ReleaseStatus.Tests ( tests ) where

import Test.MusicBrainz

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz
import MusicBrainz.Data

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddReleaseStatus
        , testResolveReference
        ]

--------------------------------------------------------------------------------
testAddReleaseStatus :: Test
testAddReleaseStatus = testCase "Can add ReleaseStatus" $ do
  CommonTests.testAdd official


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve ReleaseStatus" $ do
  CommonTests.testResolveReference (add official) entityRef


--------------------------------------------------------------------------------
official :: ReleaseStatus
official = ReleaseStatus { releaseStatusName = "Official" }
