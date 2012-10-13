{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Artist.Tests ( tests ) where

import Data.Maybe (fromJust)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

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
                     Artist { artistName = "Massive Attack"
                            , artistSortName = "Massive Attack"
                            , artistComment = ""
                            , artistBeginDate = emptyDate
                            , artistEndDate = emptyDate
                            , artistEnded = False
                            , artistGender = Nothing
                            , artistCountry = Nothing
                            , artistType = Nothing
                            }
                 }

mbTest :: MusicBrainz a -> IO a
mbTest = runMb databaseSettings
  where databaseSettings = defaultConnectInfo { connectDatabase = "musicbrainz_nes"
                                              , connectUser = "musicbrainz"
                                              }
