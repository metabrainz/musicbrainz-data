{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.ReleaseStatus.Tests ( tests ) where

import Test.MusicBrainz

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz.Class.Add
import MusicBrainz.Entity
import MusicBrainz.Release

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
