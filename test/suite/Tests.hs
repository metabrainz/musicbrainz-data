{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (forM_)

import           Test.Framework (defaultMain, testGroup)

import qualified MusicBrainz.Data.Artist.Tests
import qualified MusicBrainz.Data.ArtistType.Tests
import qualified MusicBrainz.Data.Country.Tests
import qualified MusicBrainz.Data.Edit.Tests
import qualified MusicBrainz.Data.Editor.Tests
import qualified MusicBrainz.Data.Gender.Tests
import qualified MusicBrainz.Data.Label.Tests
import qualified MusicBrainz.Data.LabelType.Tests
import qualified MusicBrainz.Data.Language.Tests
import qualified MusicBrainz.Data.Recording.Tests
import qualified MusicBrainz.Data.Relationship.Tests
import qualified MusicBrainz.Data.Release.Tests
import qualified MusicBrainz.Data.ReleaseGroup.Tests
import qualified MusicBrainz.Data.ReleaseGroupType.Tests
import qualified MusicBrainz.Data.Script.Tests
import qualified MusicBrainz.Schema.Tests
import qualified MusicBrainz.Types.Tests

import           MusicBrainz
import           Test.MusicBrainz (mbTest)

main :: IO ()
main = cleanState >> defaultMain tests
  where
    tests = [ testGroup "MusicBrainz.Data.Artist"
                MusicBrainz.Data.Artist.Tests.tests
            , testGroup "MusicBrainz.Data.ArtistType.Tests"
                MusicBrainz.Data.ArtistType.Tests.tests
            , testGroup "MusicBrainz.Data.Country.Tests"
                MusicBrainz.Data.Country.Tests.tests
            , testGroup "MusicBrainz.Data.Edit"
                MusicBrainz.Data.Edit.Tests.tests
            , testGroup "MusicBrainz.Data.Editor"
                MusicBrainz.Data.Editor.Tests.tests
            , testGroup "MusicBrainz.Data.Gender"
                MusicBrainz.Data.Gender.Tests.tests
            , testGroup "MusicBrainz.Data.Label"
                MusicBrainz.Data.Label.Tests.tests
            , testGroup "MusicBrainz.Data.LabelType"
                MusicBrainz.Data.LabelType.Tests.tests
            , testGroup "MusicBrainz.Data.Language"
                MusicBrainz.Data.Language.Tests.tests
            , testGroup "MusicBrainz.Data.Recording"
                MusicBrainz.Data.Recording.Tests.tests
            , testGroup "MusicBrainz.Data.Relationship.Tests"
                MusicBrainz.Data.Relationship.Tests.tests
            , testGroup "MusicBrainz.Data.Release"
                MusicBrainz.Data.Release.Tests.tests
            , testGroup "MusicBrainz.Data.ReleaseGroup"
                MusicBrainz.Data.ReleaseGroup.Tests.tests
            , testGroup "MusicBrainz.Data.ReleaseGroupType"
                MusicBrainz.Data.ReleaseGroupType.Tests.tests
            , testGroup "MusicBrainz.Data.Script.Tests"
                MusicBrainz.Data.Script.Tests.tests
            , testGroup "MusicBrainz.Schema.Tests"
                MusicBrainz.Schema.Tests.tests
            , testGroup "MusicBrainz.Types"
                MusicBrainz.Types.Tests.tests
            ]
    cleanState = mbTest $ forM_
      [ "SET client_min_messages TO warning"
      , "TRUNCATE artist_type CASCADE"
      , "TRUNCATE country CASCADE"
      , "TRUNCATE editor CASCADE"
      , "TRUNCATE gender CASCADE"
      , "ALTER SEQUENCE revision_revision_id_seq RESTART 1"
      , "COMMIT"
      ] $ \q -> execute q ()
