{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions to work with 'ReleaseStatus'. -}
module MusicBrainz.Data.ReleaseStatus ( addReleaseStatus ) where

import Control.Applicative
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz

--------------------------------------------------------------------------------
{-| Add a new 'ReleaseStatus' to the list of known release statuses in
MusicBrainz.-}
addReleaseStatus :: ReleaseStatus -> MusicBrainz (Entity ReleaseStatus)
addReleaseStatus rs = head <$>
  query [sql| INSERT INTO release_status (name) VALUES (?)
              RETURNING id, name |] rs
