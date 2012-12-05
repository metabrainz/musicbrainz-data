{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.ReleaseGroupType.Tests ( tests ) where

import Test.MusicBrainz
import Test.MusicBrainz.Repository (compilation)

import MusicBrainz
import MusicBrainz.Data

import qualified MusicBrainz.Data.ClassTests as ClassTests

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddPrimaryReleaseGroupType
        , testResolvePrimaryReference
        , testAddSecondaryReleaseGroupType
        , testResolveSecondaryReference
        ]

--------------------------------------------------------------------------------
testAddPrimaryReleaseGroupType :: Test
testAddPrimaryReleaseGroupType = testCase "Can add ReleaseGroupTypes" $ mbTest $ do
  ClassTests.testAdd album


--------------------------------------------------------------------------------
testResolvePrimaryReference :: Test
testResolvePrimaryReference = testCase "Can resolve ReleaseGroupTypes" $ mbTest $ do
  ClassTests.testResolveReference (add album) entityRef


--------------------------------------------------------------------------------
album :: ReleaseGroupType Primary
album = ReleaseGroupType { releaseGroupTypeName = "Album" }


--------------------------------------------------------------------------------
testAddSecondaryReleaseGroupType :: Test
testAddSecondaryReleaseGroupType = testCase "Can add ReleaseGroupTypes" $ mbTest $ do
  ClassTests.testAdd compilation


--------------------------------------------------------------------------------
testResolveSecondaryReference :: Test
testResolveSecondaryReference = testCase "Can resolve ReleaseGroupTypes" $ mbTest $ do
  ClassTests.testResolveReference (add compilation) entityRef

