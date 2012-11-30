{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions to work with 'Gender's. -}
module MusicBrainz.Data.Gender ( ) where

import Control.Applicative
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz
import MusicBrainz.Data.Add
import MusicBrainz.Data.FindLatest

--------------------------------------------------------------------------------
instance Add Gender where
  add gender = head <$>
    query [sql| INSERT INTO gender (name) VALUES (?)
                RETURNING id, name |] gender


--------------------------------------------------------------------------------
instance ResolveReference Gender where
  resolveReference genderId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM gender WHERE id = ? |] (Only genderId)
