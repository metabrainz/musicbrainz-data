{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Artist.Tests ( tests ) where

import Control.Applicative
import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Repository (uk, acid2, male, person)

import MusicBrainz
import MusicBrainz.Data.Artist
import MusicBrainz.Data.Edit
import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.Editor (register)

import qualified MusicBrainz.Data.ArtistType as ArtistType
import qualified MusicBrainz.Data.Country as Country
import qualified MusicBrainz.Data.Gender as Gender

tests :: [Test]
tests = [ testCreateFindLatest
        , testUpdate
        ]

testCreateFindLatest :: Test
testCreateFindLatest = testCase "findLatest when artist exists" $ mbTest $ do
  editor <- register acid2

  country <- entityRef <$> Country.addCountry uk
  maleRef <- entityRef <$> Gender.addGender male
  personRef <- entityRef <$> ArtistType.addArtistType person

  created <- create (entityRef editor) (ArtistTree $ expected country maleRef personRef)
  Just found <- findLatest (coreMbid created)

  liftIO $ found @?= created
  where
    expected country gender type' = Artist
      { artistName = "Freddie Mercury"
      , artistSortName = "Mercury, Freddie"
      , artistComment = "Of queen"
      , artistBeginDate =
          PartialDate (Just 1946) (Just 9) (Just 5)
      , artistEndDate =
          PartialDate (Just 1991) (Just 11) (Just 24)
      , artistEnded = True
      , artistGender = Just $ gender
      , artistCountry = Just $ country
      , artistType = Just $ type'
      }

testUpdate :: Test
testUpdate = testCase "update does change artist" $ mbTest $ do
  editor <- entityRef <$> register acid2

  created <- create editor (ArtistTree startWith)
  let artistId = coreMbid created

  newRev <- update editor (coreRevision created) (ArtistTree expected)

  editId <- openEdit
  includeRevision editId newRev
  apply editId

  Just found <- findLatest artistId
  liftIO $ coreData found @?= expected

  parents <- revisionParents newRev
  liftIO $
    assertBool "The old revision is a direct parent of the new revision" $
      parents == Set.singleton (coreRevision created)

  where
    startWith = Artist { artistName = "Freddie Mercury"
                       , artistSortName = "Mercury, Freddie"
                       , artistComment = "Of queen"
                       , artistBeginDate =
                           PartialDate (Just 1946) (Just 9) (Just 5)
                       , artistEndDate =
                           PartialDate (Just 1991) (Just 11) (Just 24)
                       , artistEnded = True
                       , artistGender = Nothing
                       , artistCountry = Nothing
                       , artistType = Nothing
                       }
    expected = startWith { artistName = "LAX is boring"
                         , artistSortName = "I want to go home"
                         }
