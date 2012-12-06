{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions to work with 'MediumFormat's. -}
module MusicBrainz.Data.MediumFormat ( ) where

import Control.Applicative
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz
import MusicBrainz.Data.Add
import MusicBrainz.Data.FindLatest

--------------------------------------------------------------------------------
instance Add MediumFormat where
  add mf = head <$>
    query [sql| INSERT INTO medium_format (name) VALUES (?) RETURNING id, name |]
      mf


--------------------------------------------------------------------------------
instance ResolveReference MediumFormat where
  resolveReference mfId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM medium_format WHERE id = ? |] (Only mfId)
