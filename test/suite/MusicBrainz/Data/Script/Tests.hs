{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Script.Tests ( tests ) where

import Test.MusicBrainz
import Test.MusicBrainz.Repository (latin)

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz
import MusicBrainz.Data

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddScript
        , testResolveReference
        ]

--------------------------------------------------------------------------------
testAddScript :: Test
testAddScript = testCase "Can add Scripts" $ mbTest $ do
  CommonTests.testAdd latin


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve Scripts" $ mbTest $ do
  CommonTests.testResolveReference (add latin) entityRef
