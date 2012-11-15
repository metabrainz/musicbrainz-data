{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.ReleaseGroup.Tests
    ( tests ) where

import Control.Applicative

import Test.MusicBrainz
import Test.MusicBrainz.Data
import Test.MusicBrainz.Repository (portishead, dummy, acid2)

import MusicBrainz
import MusicBrainz.Data.Editor
import MusicBrainz.Data.Create
import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.ReleaseGroup ()

tests :: [Test]
tests = [ testFindLatest
        ]

testFindLatest :: Test
testFindLatest = testCase "findLatest when release group exists" $ mbTest $ do
  editor <- entityRef <$> register acid2
  ac <- singleArtistAc editor portishead

  created <- create editor (ReleaseGroupTree $ dummy ac)
  found <- findLatest (coreRef created)
  liftIO $ found @?= created
