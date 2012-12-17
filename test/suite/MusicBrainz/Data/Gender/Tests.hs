{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Gender.Tests ( tests ) where

import Test.MusicBrainz
import Test.MusicBrainz.Repository (male)

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz
import MusicBrainz.Data

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddGender
        , testResolveReference
        ]

--------------------------------------------------------------------------------
testAddGender :: Test
testAddGender = testCase "Can add Genders" $ mbTest $ do
  CommonTests.testAdd male


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve Genders" $ mbTest $ do
  CommonTests.testResolveReference (add male) entityRef
