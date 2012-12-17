{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.ArtistType.Tests ( tests ) where

import Test.MusicBrainz
import Test.MusicBrainz.Repository (person)

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz
import MusicBrainz.Data

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddArtistType
        , testResolveReference
        ]

--------------------------------------------------------------------------------
testAddArtistType :: Test
testAddArtistType = testCase "Can add ArtistTypes" $ mbTest $ do
  CommonTests.testAdd person


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve ArtistTypes" $ mbTest $ do
  CommonTests.testResolveReference (add person) entityRef
