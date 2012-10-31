{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions to work with 'ReleasePackaging'. -}
module MusicBrainz.Data.ReleasePackaging ( addReleasePackaging ) where

import Control.Applicative
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz

--------------------------------------------------------------------------------
{-| Add a new 'ReleasePackaging' to the list of known types of release packaging
in MusicBrainz. -}
addReleasePackaging :: ReleasePackaging -> MusicBrainz (Entity ReleasePackaging)
addReleasePackaging rp = head <$>
  query [sql| INSERT INTO release_packaging (name) VALUES (?)
              RETURNING id, name |] rp
