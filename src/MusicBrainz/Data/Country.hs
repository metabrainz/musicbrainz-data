{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MusicBrainz.Data.Country ( addCountry ) where

import Control.Applicative
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz

addCountry :: Country -> MusicBrainz (Entity Country)
addCountry country = head <$>
  query [sql| INSERT INTO country (iso_code, name) VALUES (?, ?)
              RETURNING id, iso_code, name |] country
