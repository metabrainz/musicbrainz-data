{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Script.Tests ( tests ) where

import Test.MusicBrainz
import Test.MusicBrainz.Repository (latin)

import MusicBrainz
import MusicBrainz.Data

import qualified MusicBrainz.Data.ClassTests as ClassTests

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddScript
        , testResolveReference
        ]

--------------------------------------------------------------------------------
testAddScript :: Test
testAddScript = testCase "Can add Scripts" $ mbTest $ do
  ClassTests.testAdd latin


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve Scripts" $ mbTest $ do
  ClassTests.testResolveReference (add latin) entityRef
