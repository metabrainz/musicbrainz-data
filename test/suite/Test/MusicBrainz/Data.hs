{-# LANGUAGE OverloadedStrings #-}
module Test.MusicBrainz.Data where

import           MusicBrainz

import qualified MusicBrainz.Data.Artist as Artist
import qualified MusicBrainz.Data.ArtistCredit as ArtistCredit

singleArtistAc :: Ref Editor -> Artist -> MusicBrainz (Ref ArtistCredit)
singleArtistAc editor artist =
  Artist.create editor (ArtistTree artist) >>= ArtistCredit.getRef . liftAc
  where liftAc a = [ ArtistCreditName
                            { acnArtist = ArtistRef $ coreMbid a
                            , acnName = artistName (coreData a)
                            , acnJoinPhrase = ""
                            }
                   ]

