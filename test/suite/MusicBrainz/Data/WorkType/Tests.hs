{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.WorkType.Tests ( tests ) where

import Test.MusicBrainz

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz
import MusicBrainz.Data

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddWorkType
        , testResolveReference
        ]

--------------------------------------------------------------------------------
testAddWorkType :: Test
testAddWorkType = testCase "Can add WorkTypes" $ mbTest $ do
  CommonTests.testAdd ballet


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve WorkTypes" $ mbTest $ do
  CommonTests.testResolveReference (add ballet) entityRef


--------------------------------------------------------------------------------
ballet :: WorkType
ballet = WorkType { workTypeName = "ballet" }
