{-# LANGUAGE OverloadedStrings #-}
module Test.MusicBrainz.Data where

import qualified Data.Set as Set

import           MusicBrainz

import qualified MusicBrainz.Data.Artist as Artist
import qualified MusicBrainz.Data.ArtistCredit as ArtistCredit

singleArtistAc :: Ref Editor -> Artist -> MusicBrainz (Ref ArtistCredit)
singleArtistAc editor artist =
  Artist.create editor ArtistTree { artistData = artist
                                  , artistRelationships = Set.empty
                                  , artistAliases =  Set.empty
                                  , artistIpiCodes = Set.empty
                                  , artistAnnotation = ""
                                  } >>= ArtistCredit.getRef . liftAc
  where liftAc a = [ ArtistCreditName
                            { acnArtist = coreRef a
                            , acnName = artistName (coreData a)
                            , acnJoinPhrase = ""
                            }
                   ]

