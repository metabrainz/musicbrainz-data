{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions to work with 'LabelType's. -}
module MusicBrainz.Data.LabelType ( ) where

import Control.Applicative
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz hiding (labelType)
import MusicBrainz.Data.Add
import MusicBrainz.Data.FindLatest

--------------------------------------------------------------------------------
instance Add LabelType where
  add labelType = head <$>
    query [sql| INSERT INTO label_type (name) VALUES (?)
                RETURNING id, name |] labelType


--------------------------------------------------------------------------------
instance ResolveReference LabelType where
  resolveReference labelTypeId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM label_type WHERE id = ? |] (Only labelTypeId)
