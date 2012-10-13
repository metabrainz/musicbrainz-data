{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions for interacting with MusicBrainz artists in the database. -}
module MusicBrainz.Data.Artist
    ( -- * Reading artists
      findLatestByMbid
    ) where

import Control.Applicative
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ
import MusicBrainz

{-| Attempt to find the latest revision of an artist, by a given MBID. If
there is no artist with this MBID, then 'Nothing' is returned. This function
will also follow redirection chains, so the returned entity may have a
/different/ MBID than the input. -}
findLatestByMbid :: MBID Artist -> MusicBrainz (Maybe (CoreEntity Artist))
findLatestByMbid mbid = listToMaybe <$> query q (Only mbid)
  where q = [sql|
    SELECT artist_id, revision_id,
      name.name, sort_name.name, comment,
      begin_date_year, begin_date_month, begin_date_day,
      end_date_year, end_date_month, end_date_day, ended,
      gender_id, artist_type_id, country_id
    FROM artist
    JOIN artist_revision USING (artist_id)
    JOIN artist_tree USING (artist_tree_id)
    JOIN artist_data USING (artist_data_id)
    JOIN artist_name name ON (artist_data.name = name.id)
    JOIN artist_name sort_name ON (artist_data.sort_name = sort_name.id)
    WHERE artist_id = ?
      AND revision_id = master_revision_id  |]
