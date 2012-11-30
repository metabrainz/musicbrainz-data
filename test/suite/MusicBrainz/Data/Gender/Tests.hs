{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Gender.Tests ( tests ) where

import Test.MusicBrainz
import Test.MusicBrainz.Repository (male)

import MusicBrainz
import MusicBrainz.Data

import qualified MusicBrainz.Data.ClassTests as ClassTests

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddGender
        , testResolveReference
        ]

--------------------------------------------------------------------------------
testAddGender :: Test
testAddGender = testCase "Can add Genders" $ mbTest $ do
  ClassTests.testAdd male


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve Genders" $ mbTest $ do
  ClassTests.testResolveReference (add male) entityRef
