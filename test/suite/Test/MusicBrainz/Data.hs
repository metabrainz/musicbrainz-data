{-# LANGUAGE OverloadedStrings #-}
module Test.MusicBrainz.Data where

import Data.Monoid (mempty)

import MusicBrainz
import MusicBrainz.Data
import MusicBrainz.Data.Edit

import qualified MusicBrainz.Data.ArtistCredit as ArtistCredit

--------------------------------------------------------------------------------
singleArtistAc :: Ref Editor -> Artist -> MusicBrainz (Ref ArtistCredit)
singleArtistAc editor artist = do
  editId <- openEdit
  artistRev <- withEdit editId (create editor acTree)
  apply editId
  viewRevision artistRev >>= ArtistCredit.getRef . liftAc
  where
    liftAc a = [ ArtistCreditName
                        { acnArtist = coreRef a
                        , acnName = artistName (coreData a)
                        , acnJoinPhrase = ""
                        }
               ]
    acTree = ArtistTree { artistData = artist
                        , artistRelationships = mempty
                        , artistAliases = mempty
                        , artistIpiCodes = mempty
                        , artistAnnotation = ""
                        }
