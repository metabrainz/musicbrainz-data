{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Artist.Tests ( tests ) where

import Data.Maybe (fromJust)
import Test.MusicBrainz

import MusicBrainz
import MusicBrainz.Data.Artist
import MusicBrainz.Data.Editor (findEditorByName)

tests :: [Test]
tests = [ testFindLatestByMbid
        , testCreate
        ]

testFindLatestByMbid :: Test
testFindLatestByMbid = testCase "findLatestByMbid when artist exists" $
  mbTest (findLatestByMbid knownArtistId) >>= (@?= expected)
  where
    knownArtistId = fromJust $ parseMbid "206094f7-eea0-4f37-a4c2-97c506f5f560"
    expected = Just
      CoreEntity { coreMbid = knownArtistId
                 , coreRevision = RevisionRef 1
                 , coreData =
                     Artist { artistName = "Freddie Mercury"
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
                 }


testCreate :: Test
testCreate = testCase "create a new artist" $ do
  Just created <- mbTest $ do
    Just editor <- findEditorByName "acid2"
    created <-create (entityRef editor) expected
    findLatestByMbid (coreMbid created)
  coreData created @?= expected
  where
    knownArtistId = fromJust $ parseMbid "206094f7-eea0-4f37-a4c2-97c506f5f560"
    expected = Artist { artistName = "Darrel Fitton"
                      , artistSortName = "Fitton, Darrel"
                      , artistComment = "Performs as Bola"
                      , artistBeginDate = emptyDate
                      , artistEndDate = emptyDate
                      , artistEnded = False
                      , artistGender = Just $ GenderRef 1
                      , artistCountry = Just $ CountryRef 1
                      , artistType = Just $ ArtistTypeRef 2
                      }
