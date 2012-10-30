{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Recording.Tests
    ( tests ) where

import Control.Applicative

import Test.MusicBrainz
import Test.MusicBrainz.Data (singleArtistAc)
import Test.MusicBrainz.Repository (portishead)

import MusicBrainz
import MusicBrainz.Data.Editor (findEditorByName)
import MusicBrainz.Data.FindLatest

import qualified MusicBrainz.Data.Recording as Recording

tests :: [Test]
tests = [ testCreateFindLatest
        ]

testCreateFindLatest :: Test
testCreateFindLatest = testCase "findLatest when recording exists" $ do
  (created, Just found) <- mbTest $ do
    Just editor <- fmap entityRef <$> findEditorByName "acid2"
    ac <- singleArtistAc editor portishead

    created <- Recording.create editor (expected ac)
    found <- findLatest (coreMbid created)

    return (created, found)

  found @?= created
  where
    expected ac = Recording { recordingName = "Mysterons"
                            , recordingComment = ""
                            , recordingArtistCredit = ac
                            , recordingDuration = 64936
                            }
