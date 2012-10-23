{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions for interacting with MusicBrainz artists in the database. -}
module MusicBrainz.Data.Artist
    ( -- * Editing artists
      create
    ) where

import Control.Applicative
import Control.Monad (void)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ

import MusicBrainz
import MusicBrainz.Data.FindLatest

--------------------------------------------------------------------------------
instance FindLatest Artist where
  findLatest mbid = listToMaybe <$> query q (Only mbid)
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


--------------------------------------------------------------------------------
{-| Create an entirely new artist, returning the final 'CoreEntity' as it is
in the database. -}
create :: Ref Editor -> Artist -> MusicBrainz (CoreEntity Artist)
create editor artist = withTransaction $ do
  artistTreeId <- findOrInsertArtistData >>= findOrInsertArtistTree
  artistId <- reserveArtist
  revisionId <- newRevision >>= newArtistRevision artistId artistTreeId
  linkRevision artistId revisionId
  return CoreEntity { coreMbid = artistId
                    , coreRevision = revisionId
                    , coreData = artist
                    }
  where
    selectId = fmap (fromOnly . head)

    findOrInsertArtistData :: MusicBrainz Int
    findOrInsertArtistData = selectId $
      query [sql| SELECT find_or_insert_artist_data(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) |]
        artist

    findOrInsertArtistTree dataId = selectId $
      query [sql| SELECT find_or_insert_artist_tree(?) |]
        (Only dataId)

    newRevision :: MusicBrainz (Ref (Revision Artist))
    newRevision = selectId $
      query [sql| INSERT INTO revision (editor_id) VALUES (?) RETURNING revision_id |]
        (Only editor)

    reserveArtist :: MusicBrainz (MBID Artist)
    reserveArtist = selectId $
      query_ [sql| INSERT INTO artist (master_revision_id) VALUES (-1) RETURNING artist_id |]

    newArtistRevision :: MBID Artist -> Int -> Ref (Revision Artist) -> MusicBrainz (Ref (Revision Artist))
    newArtistRevision artistId artistTreeId revisionId = selectId $
      query [sql| INSERT INTO artist_revision (artist_id, revision_id, artist_tree_id) VALUES (?, ?, ?) RETURNING revision_id |]
        (artistId, revisionId, artistTreeId)

    linkRevision :: MBID Artist -> Ref (Revision Artist) -> MusicBrainz ()
    linkRevision artistId revisionId = void $
      execute [sql| UPDATE artist SET master_revision_id = ? WHERE artist_id = ? |] (revisionId, artistId)
