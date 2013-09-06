{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Language.Tests ( tests ) where

import Test.MusicBrainz
import Test.MusicBrainz.Repository (english)

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz.Versioning

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddLanguage
        , testResolveReference
        ]

--------------------------------------------------------------------------------
testAddLanguage :: Test
testAddLanguage = testCase "Can add Languages" $ do
  CommonTests.testAdd english


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve Languages" $ do
  CommonTests.testResolveReference (add english) entityRef
