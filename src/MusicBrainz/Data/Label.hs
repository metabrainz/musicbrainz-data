{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions for interacting with MusicBrainz labels in the database. -}
module MusicBrainz.Data.Label
    ( ) where

import Control.Applicative
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ

import MusicBrainz
import MusicBrainz.Data.FindLatest

instance FindLatest Label where
  findLatest mbid = listToMaybe <$> query q (Only mbid)
    where q = [sql|
       SELECT label_id, revision_id,
        name.name, sort_name.name, comment,
        begin_date_year, begin_date_month, begin_date_day,
        end_date_year, end_date_month, end_date_day,
        ended, label_type_id, label_code
      FROM label
      JOIN label_revision USING (label_id)
      JOIN label_tree USING (label_tree_id)
      JOIN label_data USING (label_data_id)
      JOIN label_name name ON (label_data.name = name.id)
      JOIN label_name sort_name ON (label_data.sort_name = sort_name.id)
      WHERE label_id = ?
        AND revision_id = master_revision_id  |]
