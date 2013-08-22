{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.AliasType.Tests ( tests ) where

import Test.MusicBrainz

import qualified Test.MusicBrainz.CommonTests as CommonTests

import MusicBrainz.Alias
import MusicBrainz.Artist
import MusicBrainz.Class.Add
import MusicBrainz.Entity
import MusicBrainz.Label
import MusicBrainz.Work

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testAddAliasType
        , testResolveAliasType
        ]

--------------------------------------------------------------------------------
testAddAliasType :: Test
testAddAliasType = testGroup "Can add AliasTypes"
    [ testCase "Artist alias" $ CommonTests.testAdd (testAlias :: AliasType Artist)
    , testCase "Label alias" $ CommonTests.testAdd (testAlias :: AliasType Label)
    , testCase "Work alias" $ CommonTests.testAdd (testAlias :: AliasType Work)
    ]


--------------------------------------------------------------------------------
testResolveAliasType :: Test
testResolveAliasType = testGroup "Can resolve AliasTypes"
    [ testCase "Artist alias" $ go (testAlias :: AliasType Artist)
    , testCase "Label alias" $ go (testAlias :: AliasType Label)
    , testCase "Work alias" $ go (testAlias :: AliasType Work)
    ]
  where
    go a = CommonTests.testResolveReference (add a) entityRef

--------------------------------------------------------------------------------
-- We don't specify the type of the alias type so the tests can refine the type
-- later.
testAlias :: AliasType a
testAlias = AliasType { aliasTypeName = "Search hint" }
