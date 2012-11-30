{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Relationship.Tests ( tests ) where

import Test.MusicBrainz

import MusicBrainz

import qualified MusicBrainz.Data.ClassTests as ClassTests

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddRelationshipType
        ]

--------------------------------------------------------------------------------
testAddRelationshipType :: Test
testAddRelationshipType = testCase "Can add RelationshipTypes" $ mbTest $ do
  ClassTests.testAdd RelationshipType { relName = "member of" }
