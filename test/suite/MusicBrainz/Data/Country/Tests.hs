{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Country.Tests ( tests ) where

import Test.MusicBrainz
import Test.MusicBrainz.Repository (uk)

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz
import MusicBrainz.Data

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddCountry
        , testResolveReference
        ]

--------------------------------------------------------------------------------
testAddCountry :: Test
testAddCountry = testCase "Can add Countrys" $ mbTest $ do
  CommonTests.testAdd uk


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve Countrys" $ mbTest $ do
  CommonTests.testResolveReference (add uk) entityRef
