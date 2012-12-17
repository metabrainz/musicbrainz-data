{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Language.Tests ( tests ) where

import Test.MusicBrainz
import Test.MusicBrainz.Repository (english)

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz
import MusicBrainz.Data

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddLanguage
        , testResolveReference
        ]

--------------------------------------------------------------------------------
testAddLanguage :: Test
testAddLanguage = testCase "Can add Languages" $ mbTest $ do
  CommonTests.testAdd english


--------------------------------------------------------------------------------
testResolveReference :: Test
testResolveReference = testCase "Can resolve Languages" $ mbTest $ do
  CommonTests.testResolveReference (add english) entityRef
