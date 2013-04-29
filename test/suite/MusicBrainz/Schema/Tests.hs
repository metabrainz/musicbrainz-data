{-# LANGUAGE OverloadedStrings #-}

{-| This module doesn't test much, because most schema tests are already
handled by other tests. This stuff is the edge-case/error stuff. -}
module MusicBrainz.Schema.Tests
    ( tests ) where

import Database.PostgreSQL.Simple hiding (query_)
import Database.PostgreSQL.Simple.FromField (ResultError(..))

import Test.MusicBrainz
import MusicBrainz

tests :: [Test]
tests = [ testGroup "MBID Parsing"
            [ testSelectNullMBID
            , testSelectNonUuidMBID
            ]
        ]

testSelectNullMBID :: Test
testSelectNullMBID = testCase "An MBID field must be not-null" $
  assertException expectNull $ nullQuery
  where nullQuery :: MusicBrainz [Only (MBID Artist)]
        nullQuery = query_ "SELECT null"
        expectNull (UnexpectedNull _ _ _ _ _) = Just True
        expectNull _ = Nothing

testSelectNonUuidMBID :: Test
testSelectNonUuidMBID = testCase "An MBID field must be a PostgreSQL uuid" $
  assertException expectIncompatible $ incompatQuery
  where incompatQuery :: MusicBrainz [Only (MBID Artist)]
        incompatQuery = query_ "SELECT 5"
        expectIncompatible (Incompatible _ _ _ _ _) = Just True
        expectIncompatible _ = Nothing

