{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Recording.Tests
    ( tests ) where

import Test.MusicBrainz
import Test.MusicBrainz.Repository (portishead)

import MusicBrainz
import MusicBrainz.Data.Editor (findEditorByName)
import MusicBrainz.Data.FindLatest

import qualified MusicBrainz.Data.Artist as Artist
import qualified MusicBrainz.Data.ArtistCredit as ArtistCredit
import qualified MusicBrainz.Data.Recording as Recording

tests :: [Test]
tests = [ testCreateFindLatest
        ]

testCreateFindLatest :: Test
testCreateFindLatest = testCase "findLatest when recording exists" $ do
  (created, Just found) <- mbTest $ do
    Just editor <- findEditorByName "acid2"

    artist <- Artist.create (entityRef editor) portishead

    ac <- ArtistCredit.getRef
            [ ArtistCreditName { acnArtist = ArtistRef $ coreMbid artist
                               , acnName = artistName (coreData artist)
                               , acnJoinPhrase = ""
                               }
            ]

    created <- Recording.create (entityRef editor) (expected ac)
    found <- findLatest (coreMbid created)

    return (created, found)

  found @?= created
  where
    expected ac = Recording { recordingName = "Mysterons"
                            , recordingComment = ""
                            , recordingArtistCredit = ac
                            , recordingDuration = 64936
                            }
