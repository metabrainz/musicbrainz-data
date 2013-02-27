{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Concurrent.Chan
import Control.Monad (forM_)
import Data.Configurator
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.IO.Error

import Test.Framework (buildTest, defaultMainWithOpts, interpretArgsOrExit, ropt_threads)

import qualified MusicBrainz.Data.AliasType.Tests
import qualified MusicBrainz.Data.Artist.Tests
import qualified MusicBrainz.Data.ArtistCredit.Tests
import qualified MusicBrainz.Data.ArtistType.Tests
import qualified MusicBrainz.Data.Country.Tests
import qualified MusicBrainz.Data.Edit.Tests
import qualified MusicBrainz.Data.Editor.Tests
import qualified MusicBrainz.Data.Gender.Tests
import qualified MusicBrainz.Data.Label.Tests
import qualified MusicBrainz.Data.LabelType.Tests
import qualified MusicBrainz.Data.Language.Tests
import qualified MusicBrainz.Data.MediumFormat.Tests
import qualified MusicBrainz.Data.Recording.Tests
import qualified MusicBrainz.Data.Relationship.Tests
import qualified MusicBrainz.Data.Release.Tests
import qualified MusicBrainz.Data.ReleaseGroup.Tests
import qualified MusicBrainz.Data.ReleaseGroupType.Tests
import qualified MusicBrainz.Data.ReleasePackaging.Tests
import qualified MusicBrainz.Data.ReleaseStatus.Tests
import qualified MusicBrainz.Data.Script.Tests
import qualified MusicBrainz.Data.Url.Tests
import qualified MusicBrainz.Data.Work.Tests
import qualified MusicBrainz.Data.WorkType.Tests
import qualified MusicBrainz.Schema.Tests
import qualified MusicBrainz.Types.Tests

import MusicBrainz
import Test.MusicBrainz (TestEnvironment(..), execTest, testGroup)

main :: IO ()
main = do
    args <- getArgs >>= interpretArgsOrExit
    defaultMainWithOpts
      [buildTest (testRunner (ropt_threads args) (testGroup "All tests" tests))]
      args
  where
    testRunner contexts t = do
      testConfig <- load [ Required "test.cfg" ] `catchIOError`
        (\e -> if isDoesNotExistError e
                 then error "test.cfg not found. Please add a test.cfg file, see test.cfg.example for more information."
                 else ioError e)

      let opt key def = lookupDefault (def defaultConnectInfo) testConfig key
      conn <- ConnectInfo
                <$> opt "host" connectHost
                <*> opt "port" connectPort
                <*> opt "user" connectUser
                <*> opt "password" connectPassword
                <*> opt "database" connectDatabase

      ctxChan <- newChan
      forM_ [0..fromMaybe 0 contexts] $
        const (openContext conn >>= writeChan ctxChan)

      ctx <- readChan ctxChan
      runMbContext ctx cleanState
      writeChan ctxChan ctx

      execTest t (TestEnvironment ctxChan)

    tests = [ testGroup "MusicBrainz.Data.AliasType"
                MusicBrainz.Data.AliasType.Tests.tests
            , testGroup "MusicBrainz.Data.Artist"
                MusicBrainz.Data.Artist.Tests.tests
            , testGroup "MusicBrainz.Data.ArtistCredit"
                MusicBrainz.Data.ArtistCredit.Tests.tests
            , testGroup "MusicBrainz.Data.ArtistType"
                MusicBrainz.Data.ArtistType.Tests.tests
            , testGroup "MusicBrainz.Data.Country"
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
            , testGroup "MusicBrainz.Data.MediumFormat"
                MusicBrainz.Data.MediumFormat.Tests.tests
            , testGroup "MusicBrainz.Data.Recording"
                MusicBrainz.Data.Recording.Tests.tests
            , testGroup "MusicBrainz.Data.Relationship"
                MusicBrainz.Data.Relationship.Tests.tests
            , testGroup "MusicBrainz.Data.Release"
                MusicBrainz.Data.Release.Tests.tests
            , testGroup "MusicBrainz.Data.ReleaseGroup"
                MusicBrainz.Data.ReleaseGroup.Tests.tests
            , testGroup "MusicBrainz.Data.ReleaseGroupType"
                MusicBrainz.Data.ReleaseGroupType.Tests.tests
            , testGroup "MusicBrainz.Data.ReleasePackaging"
                MusicBrainz.Data.ReleasePackaging.Tests.tests
            , testGroup "MusicBrainz.Data.ReleaseStatus"
                MusicBrainz.Data.ReleaseStatus.Tests.tests
            , testGroup "MusicBrainz.Data.Script"
                MusicBrainz.Data.Script.Tests.tests
            , testGroup "MusicBrainz.Data.Url"
                MusicBrainz.Data.Url.Tests.tests
            , testGroup "MusicBrainz.Data.Work"
                MusicBrainz.Data.Work.Tests.tests
            , testGroup "MusicBrainz.Data.WorkType"
                MusicBrainz.Data.WorkType.Tests.tests
            , testGroup "MusicBrainz.Schema"
                MusicBrainz.Schema.Tests.tests
            , testGroup "MusicBrainz.Types"
                MusicBrainz.Types.Tests.tests
            ]

    cleanState = forM_
      [ "SET client_min_messages TO warning"
      , "TRUNCATE artist_type CASCADE"
      , "TRUNCATE country CASCADE"
      , "TRUNCATE editor CASCADE"
      , "TRUNCATE gender CASCADE"
      , "TRUNCATE label_type CASCADE"
      , "TRUNCATE language CASCADE"
      , "TRUNCATE link_type CASCADE"
      , "TRUNCATE medium_format CASCADE"
      , "TRUNCATE release_group_primary_type CASCADE"
      , "TRUNCATE release_group_secondary_type CASCADE"
      , "TRUNCATE release_status CASCADE"
      , "TRUNCATE script CASCADE"
      , "TRUNCATE track CASCADE"
      , "TRUNCATE work_type CASCADE"
      , "ALTER SEQUENCE revision_revision_id_seq RESTART 1"
      ] $ \q -> execute q ()
