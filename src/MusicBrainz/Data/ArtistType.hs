{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions to work with 'ArtistType's. -}
module MusicBrainz.Data.ArtistType ( ) where

import Control.Applicative
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz hiding (artistType)
import MusicBrainz.Data.Add
import MusicBrainz.Data.FindLatest

--------------------------------------------------------------------------------
instance Add ArtistType where
  add artistType = head <$>
    query [sql| INSERT INTO artist_type (name) VALUES (?)
                RETURNING id, name |] artistType


--------------------------------------------------------------------------------
instance ResolveReference ArtistType where
  resolveReference artistTypeId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM artist_type WHERE id = ? |] (Only artistTypeId)
