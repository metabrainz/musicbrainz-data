{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions to work with 'Gender's. -}
module MusicBrainz.Data.Gender ( addGender ) where

import Control.Applicative
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz

--------------------------------------------------------------------------------
{-| Add a new 'Gender' to the list of known genders in MusicBrainz. -}
addGender :: Gender -> MusicBrainz (Entity Gender)
addGender gender = head <$>
  query [sql| INSERT INTO gender (name) VALUES (?)
              RETURNING id, name |] gender
