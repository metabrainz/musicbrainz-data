{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MusicBrainz.Data.Script ( addScript ) where

import Control.Applicative
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz

addScript :: Script -> MusicBrainz (Entity Script)
addScript script = head <$>
  query [sql| INSERT INTO script (iso_code, iso_number, name) VALUES (?, ?, ?)
              RETURNING id, iso_code, iso_number, name |] script
