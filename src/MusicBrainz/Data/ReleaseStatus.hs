{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MusicBrainz.Data.ReleaseStatus ( addReleaseStatus ) where

import Control.Applicative
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz

addReleaseStatus :: ReleaseStatus -> MusicBrainz (Entity ReleaseStatus)
addReleaseStatus rs = head <$>
  query [sql| INSERT INTO release_status (name) VALUES (?)
              RETURNING id, name |] rs
