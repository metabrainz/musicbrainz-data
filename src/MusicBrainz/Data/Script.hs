{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions to work with 'Script's. -}
module MusicBrainz.Data.Script ( ) where

import Control.Applicative
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz
import MusicBrainz.Data.Add
import MusicBrainz.Data.FindLatest

--------------------------------------------------------------------------------
instance Add Script where
  add script = head <$>
    query [sql| INSERT INTO script (iso_code, iso_number, name) VALUES (?, ?, ?)
                RETURNING id, iso_code, iso_number, name |] script


--------------------------------------------------------------------------------
instance ResolveReference Script where
  resolveReference scriptRef = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM script WHERE id = ? |] (Only scriptRef)
