{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Script.Tests ( tests ) where

import Test.MusicBrainz
import Test.MusicBrainz.Repository (latin)

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz.Class.Add
import MusicBrainz.Entity
import MusicBrainz.Script ()

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddScript
        , testResolveReference
        ]

--------------------------------------------------------------------------------
testAddScript :: Test
testAddScript = testCase "Can add Scripts" $ do
  CommonTests.testAdd latin


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve Scripts" $ do
  CommonTests.testResolveReference (add latin) entityRef
