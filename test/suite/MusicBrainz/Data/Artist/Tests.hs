{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Artist.Tests ( tests ) where

import Test.MusicBrainz

import MusicBrainz
import MusicBrainz.Data.Artist
import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.Editor (findEditorByName)

tests :: [Test]
tests = [ testCreateFindLatest
        ]

testCreateFindLatest :: Test
testCreateFindLatest = testCase "findLatest when artist exists" $ do
  (created, Just found) <- mbTest $ do
    Just editor <- findEditorByName "acid2"
    created <- create (entityRef editor) expected

    found <- findLatest (coreMbid created)

    return (created, found)
  found @?= created
  where
    expected = Artist { artistName = "Freddie Mercury"
                      , artistSortName = "Mercury, Freddie"
                      , artistComment = "Of queen"
                      , artistBeginDate =
                          PartialDate (Just 1946) (Just 9) (Just 5)
                      , artistEndDate =
                          PartialDate (Just 1991) (Just 11) (Just 24)
                      , artistEnded = True
                      , artistGender = Just $ GenderRef 1
                      , artistCountry = Just $ CountryRef 1
                      , artistType = Just $ ArtistTypeRef 1
                      }
