{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.WorkType.Tests ( tests ) where

import Test.MusicBrainz

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz.Class.Add
import MusicBrainz.Entity
import MusicBrainz.Work

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddWorkType
        , testResolveReference
        ]

--------------------------------------------------------------------------------
testAddWorkType :: Test
testAddWorkType = testCase "Can add WorkTypes" $ do
  CommonTests.testAdd ballet


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve WorkTypes" $ do
  CommonTests.testResolveReference (add ballet) entityRef


--------------------------------------------------------------------------------
ballet :: WorkType
ballet = WorkType { workTypeName = "ballet" }
