{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.ReleaseGroupType.Tests ( tests ) where

import Test.MusicBrainz
import Test.MusicBrainz.Repository (compilation)

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz.Class.Add
import MusicBrainz.Entity
import MusicBrainz.ReleaseGroup

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddPrimaryReleaseGroupType
        , testResolvePrimaryReference
        , testAddSecondaryReleaseGroupType
        , testResolveSecondaryReference
        ]

--------------------------------------------------------------------------------
testAddPrimaryReleaseGroupType :: Test
testAddPrimaryReleaseGroupType = testCase "Can add ReleaseGroupTypes" $ do
  CommonTests.testAdd album


--------------------------------------------------------------------------------
testResolvePrimaryReference :: Test
testResolvePrimaryReference = testCase "Can resolve ReleaseGroupTypes" $ do
  CommonTests.testResolveReference (add album) entityRef


--------------------------------------------------------------------------------
album :: ReleaseGroupType Primary
album = ReleaseGroupType { releaseGroupTypeName = "Album" }


--------------------------------------------------------------------------------
testAddSecondaryReleaseGroupType :: Test
testAddSecondaryReleaseGroupType = testCase "Can add ReleaseGroupTypes" $ do
  CommonTests.testAdd compilation


--------------------------------------------------------------------------------
testResolveSecondaryReference :: Test
testResolveSecondaryReference = testCase "Can resolve ReleaseGroupTypes" $ do
  CommonTests.testResolveReference (add compilation) entityRef

