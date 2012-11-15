{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Recording.Tests
    ( tests ) where

import Control.Applicative

import Test.MusicBrainz
import Test.MusicBrainz.Data (singleArtistAc)
import Test.MusicBrainz.Repository (portishead, acid2)

import MusicBrainz
import MusicBrainz.Data.Create
import MusicBrainz.Data.Editor (register)
import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.Recording ()

tests :: [Test]
tests = [ testCreateFindLatest
        ]

testCreateFindLatest :: Test
testCreateFindLatest = testCase "findLatest when recording exists" $ mbTest $ do
  editor <- entityRef <$> register acid2
  ac <- singleArtistAc editor portishead

  created <- create editor (RecordingTree $ expected ac)
  found <- findLatest (coreRef created)

  liftIO $ found @?= created
  where
    expected ac = Recording { recordingName = "Mysterons"
                            , recordingComment = ""
                            , recordingArtistCredit = ac
                            , recordingDuration = 64936
                            }
