{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions to work with 'ArtistType's. -}
module MusicBrainz.Data.ArtistType ( addArtistType ) where

import Control.Applicative
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz hiding (artistType)

--------------------------------------------------------------------------------
{-| Add a new 'ArtistType' to the list of known artist types in MusicBrainz. -}
addArtistType :: ArtistType -> MusicBrainz (Entity ArtistType)
addArtistType artistType = head <$>
  query [sql| INSERT INTO artist_type (name) VALUES (?)
              RETURNING id, name |] artistType
