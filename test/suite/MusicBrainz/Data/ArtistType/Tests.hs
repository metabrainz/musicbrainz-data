{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.ArtistType.Tests ( tests ) where

import Test.MusicBrainz
import Test.MusicBrainz.Repository (person)

import MusicBrainz
import MusicBrainz.Data

import qualified MusicBrainz.Data.ClassTests as ClassTests

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddArtistType
        , testResolveReference
        ]

--------------------------------------------------------------------------------
testAddArtistType :: Test
testAddArtistType = testCase "Can add ArtistTypes" $ mbTest $ do
  ClassTests.testAdd person


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve ArtistTypes" $ mbTest $ do
  ClassTests.testResolveReference (add person) entityRef
