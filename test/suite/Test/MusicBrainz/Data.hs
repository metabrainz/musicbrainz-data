{-# LANGUAGE OverloadedStrings #-}
module Test.MusicBrainz.Data where

import qualified Data.Set as Set

import           MusicBrainz

import MusicBrainz.Data
import qualified MusicBrainz.Data.ArtistCredit as ArtistCredit

--------------------------------------------------------------------------------
singleArtistAc :: Ref Editor -> Artist -> MusicBrainz (Ref ArtistCredit)
singleArtistAc editor artist =
  create editor ArtistTree { artistData = artist
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

--------------------------------------------------------------------------------
class MinimalTree a where
  minimalTree :: a -> Tree a

instance MinimalTree Artist where
  minimalTree dat' = ArtistTree dat' Set.empty Set.empty Set.empty ""

instance MinimalTree Label where
  minimalTree dat' = LabelTree dat' Set.empty Set.empty ""

instance MinimalTree Recording where
  minimalTree dat' = RecordingTree dat' ""

instance MinimalTree Release where
  minimalTree dat' = ReleaseTree dat' ""

instance MinimalTree ReleaseGroup where
  minimalTree dat' = ReleaseGroupTree dat' ""
