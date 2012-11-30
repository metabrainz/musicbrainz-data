{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions to work with 'Country's. -}
module MusicBrainz.Data.Country ( add ) where

import Control.Applicative
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz
import MusicBrainz.Data.Add
import MusicBrainz.Data.FindLatest

--------------------------------------------------------------------------------
instance Add Country where
  add country = head <$>
    query [sql| INSERT INTO country (iso_code, name) VALUES (?, ?)
                RETURNING id, iso_code, name |] country


--------------------------------------------------------------------------------
instance ResolveReference Country where
  resolveReference countryId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM country WHERE id = ? |] (Only countryId)
