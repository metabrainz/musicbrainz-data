{-# LANGUAGE OverloadedStrings #-}
module Test.MusicBrainz.Data where

import Data.Monoid (mempty)

import MusicBrainz.Monad
import MusicBrainz.Artist
import MusicBrainz.ArtistCredit (ArtistCredit, ArtistCreditName(..), getRef)
import MusicBrainz.EditApplication
import MusicBrainz.Versioning

--------------------------------------------------------------------------------
singleArtistAc :: Ref Editor -> Artist -> MusicBrainz (Ref ArtistCredit)
singleArtistAc editor artist = do
  editId <- openEdit
  artistRev <- withEdit editId (create editor acTree)
  apply editId
  viewRevision artistRev >>= getRef . liftAc
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
                        , artistIsniCodes = mempty
                        , artistAnnotation = ""
                        }
