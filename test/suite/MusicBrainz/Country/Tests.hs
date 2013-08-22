{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Country.Tests ( tests ) where

import Test.MusicBrainz
import Test.MusicBrainz.Repository (uk)

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz.Class.Add
import MusicBrainz.Entity

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddCountry
        , testResolveReference
        ]

--------------------------------------------------------------------------------
testAddCountry :: Test
testAddCountry = testCase "Can add Countrys" $ do
  CommonTests.testAdd uk


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve Countrys" $ do
  CommonTests.testResolveReference (add uk) entityRef
