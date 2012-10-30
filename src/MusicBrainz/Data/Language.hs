{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MusicBrainz.Data.Language ( addLanguage ) where

import Control.Applicative
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz

addLanguage :: Language -> MusicBrainz (Entity Language)
addLanguage language = head <$>
  query [sql| INSERT INTO language (name, iso_code_2t, iso_code_2b, iso_code_1, iso_code_3) VALUES (?, ?, ?, ?, ?)
              RETURNING id, name, iso_code_2t, iso_code_2b, iso_code_1, iso_code_3 |] language
