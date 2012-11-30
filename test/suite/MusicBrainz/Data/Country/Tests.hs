{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Country.Tests ( tests ) where

import Test.MusicBrainz
import Test.MusicBrainz.Repository (uk)

import MusicBrainz
import MusicBrainz.Data

import qualified MusicBrainz.Data.ClassTests as ClassTests

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddCountry
        , testResolveReference
        ]

--------------------------------------------------------------------------------
testAddCountry :: Test
testAddCountry = testCase "Can add Countrys" $ mbTest $ do
  ClassTests.testAdd uk


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve Countrys" $ mbTest $ do
  ClassTests.testResolveReference (add uk) entityRef
