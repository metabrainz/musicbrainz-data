{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Artist.Tests ( tests ) where

import Control.Applicative

import Test.MusicBrainz

import MusicBrainz
import MusicBrainz.Data.Artist
import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.Editor (findEditorByName)

tests :: [Test]
tests = [ testCreateFindLatest
        , testUpdate
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

testUpdate :: Test
testUpdate = testCase "update does change artist" $ do
  (created, revised, parents) <- mbTest $ do
    Just editor <- fmap entityRef <$> findEditorByName "acid2"

    created <- create editor expected
    newRev <- update editor (coreRevision created) expected

    found <- viewRevision newRev
    parents <- revisionParents newRev
    return (created, found, parents)

  assertBool "MBID does not change over update" $
    coreMbid revised == coreMbid created

  assertBool "Revision does change over update" $
    coreData revised == expected

  assertBool "The old revision is a direct parent of the new revision" $
    parents == [coreRevision created]

  where
    startWith = Artist { artistName = "Freddie Mercury"
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
    expected = startWith { artistName = "LAX is boring"
                         , artistSortName = "I want to go home"
                         }
