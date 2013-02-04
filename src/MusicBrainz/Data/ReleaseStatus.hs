{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions to work with 'ReleaseStatus'. -}
module MusicBrainz.Data.ReleaseStatus ( ) where

import Control.Applicative
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz
import MusicBrainz.Data.Add
import MusicBrainz.Data.FindLatest

--------------------------------------------------------------------------------
instance Add ReleaseStatus where
  add rs = head <$>
    query [sql| INSERT INTO release_status (name) VALUES (?)
                RETURNING id, name |] rs


--------------------------------------------------------------------------------
instance ResolveReference ReleaseStatus where
  resolveReference releaseStatusId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM release_status WHERE id = ? |] (Only releaseStatusId)
