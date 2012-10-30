{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MusicBrainz.Data.ReleasePackaging ( addReleasePackaging ) where

import Control.Applicative
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz

addReleasePackaging :: ReleasePackaging -> MusicBrainz (Entity ReleasePackaging)
addReleasePackaging rp = head <$>
  query [sql| INSERT INTO release_packaging (name) VALUES (?)
              RETURNING id, name |] rp
