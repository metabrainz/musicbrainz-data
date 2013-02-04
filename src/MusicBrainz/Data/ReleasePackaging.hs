{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions to work with 'ReleasePackaging'. -}
module MusicBrainz.Data.ReleasePackaging ( ) where

import Control.Applicative
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz
import MusicBrainz.Data.Add
import MusicBrainz.Data.FindLatest

--------------------------------------------------------------------------------
instance Add ReleasePackaging where
  add rp = head <$>
    query [sql| INSERT INTO release_packaging (name) VALUES (?)
                RETURNING id, name |] rp


--------------------------------------------------------------------------------
instance ResolveReference ReleasePackaging where
  resolveReference releasePackagingId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM release_packaging WHERE id = ? |] (Only releasePackagingId)
