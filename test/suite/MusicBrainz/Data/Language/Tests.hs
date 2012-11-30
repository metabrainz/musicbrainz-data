{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Language.Tests ( tests ) where

import Test.MusicBrainz
import Test.MusicBrainz.Repository (english)

import MusicBrainz
import MusicBrainz.Data

import qualified MusicBrainz.Data.ClassTests as ClassTests

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddLanguage
        , testResolveReference
        ]

--------------------------------------------------------------------------------
testAddLanguage :: Test
testAddLanguage = testCase "Can add Languages" $ mbTest $ do
  ClassTests.testAdd english


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve Languages" $ mbTest $ do
  ClassTests.testResolveReference (add english) entityRef
