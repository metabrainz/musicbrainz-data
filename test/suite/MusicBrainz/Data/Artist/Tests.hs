{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Artist.Tests ( tests ) where

import Control.Applicative

import Test.MusicBrainz
import Test.MusicBrainz.Repository (uk)

import MusicBrainz
import MusicBrainz.Data.Artist
import MusicBrainz.Data.Edit
import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.Editor (findEditorByName)

import qualified MusicBrainz.Data.Country as Country

tests :: [Test]
tests = [ testCreateFindLatest
        , testUpdate
        ]

testCreateFindLatest :: Test
testCreateFindLatest = testCase "findLatest when artist exists" $ mbTest $ do
  Just editor <- findEditorByName "acid2"
  country <- Country.addCountry uk

  created <- create (entityRef editor) (expected (entityRef country))
  Just found <- findLatest (coreMbid created)

  liftIO $ found @?= created
  where
    expected country = Artist
      { artistName = "Freddie Mercury"
      , artistSortName = "Mercury, Freddie"
      , artistComment = "Of queen"
      , artistBeginDate =
          PartialDate (Just 1946) (Just 9) (Just 5)
      , artistEndDate =
          PartialDate (Just 1991) (Just 11) (Just 24)
      , artistEnded = True
      , artistGender = Just $ GenderRef 1
      , artistCountry = Just $ country
      , artistType = Just $ ArtistTypeRef 1
      }

testUpdate :: Test
testUpdate = testCase "update does change artist" $ mbTest $ do
  Just editor <- fmap entityRef <$> findEditorByName "acid2"

  created <- create editor startWith
  let artistId = coreMbid created

  newRev <- update editor (coreRevision created) expected

  editId <- openEdit
  includeRevision editId newRev
  apply editId

  Just found <- findLatest artistId
  liftIO $ coreData found @?= expected

  parents <- revisionParents newRev
  liftIO $
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
                       , artistGender = Nothing
                       , artistCountry = Nothing
                       , artistType = Nothing
                       }
    expected = startWith { artistName = "LAX is boring"
                         , artistSortName = "I want to go home"
                         }
