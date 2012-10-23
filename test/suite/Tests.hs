module Main where

import Test.Framework (defaultMain, testGroup)

import qualified MusicBrainz.Data.Artist.Tests
import qualified MusicBrainz.Data.Label.Tests
import qualified MusicBrainz.Data.Recording.Tests
import qualified MusicBrainz.Data.Release.Tests
import qualified MusicBrainz.Schema.Tests
import qualified MusicBrainz.Types.Tests

main :: IO ()
main = defaultMain tests
  where
    tests = [ testGroup "MusicBrainz.Data.Artist"
                MusicBrainz.Data.Artist.Tests.tests
            , testGroup "MusicBrainz.Data.Label"
                MusicBrainz.Data.Label.Tests.tests
            , testGroup "MusicBrainz.Data.Recording"
                MusicBrainz.Data.Recording.Tests.tests
            , testGroup "MusicBrainz.Data.Release"
                MusicBrainz.Data.Release.Tests.tests
            , testGroup "MusicBrainz.Schema.Tests"
                MusicBrainz.Schema.Tests.tests
            , testGroup "MusicBrainz.Types"
                MusicBrainz.Types.Tests.tests
            ]
