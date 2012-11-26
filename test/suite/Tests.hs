{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (forM_)

import           Test.Framework (defaultMain, testGroup)

import qualified MusicBrainz.Data.Artist.Tests
import qualified MusicBrainz.Data.Edit.Tests
import qualified MusicBrainz.Data.Editor.Tests
import qualified MusicBrainz.Data.Label.Tests
import qualified MusicBrainz.Data.Recording.Tests
import qualified MusicBrainz.Data.Release.Tests
import qualified MusicBrainz.Data.ReleaseGroup.Tests
import qualified MusicBrainz.Schema.Tests
import qualified MusicBrainz.Types.Tests

import           MusicBrainz
import           Test.MusicBrainz (mbTest)

main :: IO ()
main = cleanState >> defaultMain tests
  where
    tests = [ testGroup "MusicBrainz.Data.Artist"
                MusicBrainz.Data.Artist.Tests.tests
            , testGroup "MusicBrainz.Data.Edit"
                MusicBrainz.Data.Edit.Tests.tests
            , testGroup "MusicBrainz.Data.Editor"
                MusicBrainz.Data.Editor.Tests.tests
            , testGroup "MusicBrainz.Data.Label"
                MusicBrainz.Data.Label.Tests.tests
            , testGroup "MusicBrainz.Data.Recording"
                MusicBrainz.Data.Recording.Tests.tests
            , testGroup "MusicBrainz.Data.Release"
                MusicBrainz.Data.Release.Tests.tests
            , testGroup "MusicBrainz.Data.ReleaseGroup"
                MusicBrainz.Data.ReleaseGroup.Tests.tests
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
