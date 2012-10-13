{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Artist.Tests ( tests ) where

import Data.Maybe (fromJust)
import Test.MusicBrainz

import MusicBrainz
import MusicBrainz.Data.Artist

tests :: [Test]
tests = [ testFindLatestByMbid ]

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
