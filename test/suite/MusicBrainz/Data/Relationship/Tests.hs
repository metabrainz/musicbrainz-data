{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Relationship.Tests ( tests ) where

import Test.MusicBrainz

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddRelationshipType
        ]

--------------------------------------------------------------------------------
testAddRelationshipType :: Test
testAddRelationshipType = testCase "Can add RelationshipTypes" $ mbTest $ do
  CommonTests.testAdd RelationshipType { relName = "member of" }
