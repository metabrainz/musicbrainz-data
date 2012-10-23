{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Functions for interacting with 'Release's in the MusicBrainz database. -}
module MusicBrainz.Data.Release
    ( ) where

import Control.Applicative
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz
import MusicBrainz.Data.FindLatest

instance FindLatest Release where
  findLatest mbid = listToMaybe <$> query q (Only mbid)
    where q = [sql|
       SELECT release_id, revision_id,
         name.name, comment, artist_credit_id, release_group_id,
         date_year, date_month, date_day, country_id, script_id, language_id,
         release_packaging_id, release_status_id
      FROM release
      JOIN release_revision USING (release_id)
      JOIN release_tree USING (release_tree_id)
      JOIN release_data USING (release_data_id)
      JOIN release_name name ON (release_data.name = name.id)
      WHERE release_id = ?
        AND revision_id = master_revision_id  |]

