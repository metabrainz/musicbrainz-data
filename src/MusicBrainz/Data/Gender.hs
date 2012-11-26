{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions to work with 'Gender's. -}
module MusicBrainz.Data.Gender ( addGender ) where

import Control.Applicative
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz
import MusicBrainz.Data.FindLatest

--------------------------------------------------------------------------------
{-| Add a new 'Gender' to the list of known genders in MusicBrainz. -}
addGender :: Gender -> MusicBrainz (Entity Gender)
addGender gender = head <$>
  query [sql| INSERT INTO gender (name) VALUES (?)
              RETURNING id, name |] gender


--------------------------------------------------------------------------------
instance ResolveReference Gender where
  resolveReference genderId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM gender WHERE id = ? |] (Only genderId)
