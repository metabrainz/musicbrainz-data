{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Artist.Tests ( tests ) where

import Control.Applicative

import qualified Data.Set as Set

import Test.MusicBrainz
import Test.MusicBrainz.Repository (uk, acid2, male, person, portishead)

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
        , testRelationships
        ]

testCreateFindLatest :: Test
testCreateFindLatest = testCase "findLatest when artist exists" $ mbTest $ do
  editor <- register acid2

  country <- entityRef <$> Country.addCountry uk
  maleRef <- entityRef <$> Gender.addGender male
  personRef <- entityRef <$> ArtistType.addArtistType person

  created <- create (entityRef editor) (ArtistTree (expected country maleRef personRef) Set.empty)
  Just found <- findLatest (coreRef created)

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

  created <- create editor (ArtistTree freddie Set.empty)
  let artistId = coreRef created

  editId <- createEdit $
    update editor (coreRevision created) (ArtistTree expected Set.empty)

  apply editId

  Just found <- findLatest artistId
  liftIO $ coreData found @?= expected

  parents <- revisionParents (coreRevision found)
  liftIO $
    coreRevision created `Set.member` parents @? "Is parented to starting revision"

  where
    expected = freddie { artistName = "LAX is boring"
                       , artistSortName = "I want to go home"
                       }


testRelationships :: Test
testRelationships = testCase "update does change artist" $ mbTest $ do
  editor <- entityRef <$> register acid2

  a <- create editor (ArtistTree freddie Set.empty)
  b <- create editor (ArtistTree portishead Set.empty)

  editId <- createEdit $
    update editor (coreRevision a) (ArtistTree freddie (Set.singleton $ ArtistRelationship (coreRef b)))

  apply editId

  Just latest <- findLatest (coreRef a)
  oldRels <- viewRelationships (coreRevision a)
  newRels <- viewRelationships (coreRevision latest)

  liftIO $ do
    oldRels @?= Set.empty
    newRels @?= (Set.singleton $ ArtistRelationship (coreRef b) )

freddie :: Artist
freddie = Artist { artistName = "Freddie Mercury"
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
