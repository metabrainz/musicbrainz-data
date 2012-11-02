{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.ReleaseGroup.Tests
    ( tests ) where

import Test.MusicBrainz
import Test.MusicBrainz.Repository (portishead, dummy)

import MusicBrainz
import MusicBrainz.Data.Editor
import MusicBrainz.Data.FindLatest

import qualified MusicBrainz.Data.Artist as Artist
import qualified MusicBrainz.Data.ArtistCredit as ArtistCredit
import qualified MusicBrainz.Data.ReleaseGroup as ReleaseGroup

tests :: [Test]
tests = [ testFindLatest
        ]

testFindLatest :: Test
testFindLatest = testCase "findLatest when release group exists" $ mbTest $ do
  Just editor <- findEditorByName "acid2"
  artist <- Artist.create (entityRef editor) portishead
  ac <- ArtistCredit.getRef
          [ ArtistCreditName { acnArtist = ArtistRef $ coreMbid artist
                             , acnName = artistName (coreData artist)
                             , acnJoinPhrase = ""
                             }
          ]

  created <- ReleaseGroup.create (entityRef editor) (dummy ac)
  Just found <- findLatest (coreMbid created)
  liftIO $ found @?= created
