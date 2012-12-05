{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.WorkType.Tests ( tests ) where

import Test.MusicBrainz
import Test.MusicBrainz.Repository (person)

import MusicBrainz
import MusicBrainz.Data

import qualified MusicBrainz.Data.ClassTests as ClassTests

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddWorkType
        , testResolveReference
        ]

--------------------------------------------------------------------------------
testAddWorkType :: Test
testAddWorkType = testCase "Can add WorkTypes" $ mbTest $ do
  ClassTests.testAdd person


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve WorkTypes" $ mbTest $ do
  ClassTests.testResolveReference (add person) entityRef
