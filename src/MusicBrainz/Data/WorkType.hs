{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions to work with 'WorkType's. -}
module MusicBrainz.Data.WorkType ( ) where

import Control.Applicative
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz hiding (workType)
import MusicBrainz.Data.Add
import MusicBrainz.Data.FindLatest

--------------------------------------------------------------------------------
instance Add WorkType where
  add workType = head <$>
    query [sql| INSERT INTO work_type (name) VALUES (?)
                RETURNING id, name |] workType


--------------------------------------------------------------------------------
instance ResolveReference WorkType where
  resolveReference workTypeId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM work_type WHERE id = ? |] (Only workTypeId)
