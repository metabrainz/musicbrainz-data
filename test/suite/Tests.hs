module Main where

import Test.Framework (defaultMain, testGroup)

import qualified MusicBrainz.Data.Artist.Tests
import qualified MusicBrainz.Types.Tests

main :: IO ()
main = defaultMain tests
  where
    tests = [ testGroup "MusicBrainz.Types"
                MusicBrainz.Types.Tests.tests
            , testGroup "MusicBrainz.Data.Artist"
                MusicBrainz.Data.Artist.Tests.tests
            ]
