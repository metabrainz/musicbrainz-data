{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions for interacting with MusicBrainz artists in the database. -}
module MusicBrainz.Data.Artist
    ( -- * Working with revisions
      viewRevision
    , revisionParents

      -- * Editing artists
    , create
    , update
    ) where

import Control.Applicative
import Control.Monad (void)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ

import MusicBrainz
import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.Revision

import qualified MusicBrainz.Data.Generic.Create as GenericCreate

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
viewRevision :: Ref (Revision Artist) -> MusicBrainz (CoreEntity Artist)
viewRevision revision = head <$> query q (Only revision)
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
      WHERE revision_id = ? |]

--------------------------------------------------------------------------------
revisionParents :: Ref (Revision Artist) -> MusicBrainz [Ref (Revision Artist)]
revisionParents artistRev = map fromOnly <$> query q (Only artistRev)
  where q = [sql| SELECT parent_revision_id FROM revision_parent
                  WHERE revision_id = ? |]

--------------------------------------------------------------------------------
{-| Create an entirely new artist, returning the final 'CoreEntity' as it is
in the database. -}
create :: Ref Editor -> Artist -> MusicBrainz (CoreEntity Artist)
create = GenericCreate.create GenericCreate.Specification
    { GenericCreate.getTree = artistTree
    , GenericCreate.reserveEntity = GenericCreate.reserveEntityTable "artist"
    , GenericCreate.newEntityRevision = newArtistRevision
    , GenericCreate.linkRevision = linkRevision
    }
  where
    newArtistRevision artistId artistTreeId revisionId = selectValue $
      query [sql| INSERT INTO artist_revision (artist_id, revision_id, artist_tree_id)
                  VALUES (?, ?, ?) RETURNING revision_id |]
        (artistId, revisionId, artistTreeId)

    linkRevision artistId revisionId = void $
      execute [sql| UPDATE artist SET master_revision_id = ? WHERE artist_id = ? |] (revisionId, artistId)


--------------------------------------------------------------------------------
{-| Update the information about an artist, yielding a new revision. -}
update :: Ref Editor -> Ref (Revision Artist) -> Artist
       -> MusicBrainz (Ref (Revision Artist))
update editor baseRev artist = do
  newTree <- artistTree artist
  revisionId <- newRevision editor >>= newArtistRevision baseRev newTree
  addChild revisionId baseRev
  return revisionId

  where
    newArtistRevision :: Ref (Revision Artist) -> Ref (Tree Artist) -> Ref (Revision Artist)
                      -> MusicBrainz (Ref (Revision Artist))
    newArtistRevision parentRevision artistTreeId revisionId = selectValue $
      query [sql| INSERT INTO artist_revision (artist_id, revision_id, artist_tree_id)
                  VALUES ( (SELECT artist_id FROM artist_revision WHERE revision_id = ?)
                         , ?, ?) RETURNING revision_id |]
        (parentRevision, revisionId, artistTreeId)

    addChild childRevision parentRevision =
      execute [sql| INSERT INTO revision_parent (revision_id, parent_revision_id)
                    VALUES (?, ?) |] (childRevision, parentRevision)

--------------------------------------------------------------------------------
artistTree :: Artist -> MusicBrainz (Ref (Tree Artist))
artistTree artist = findOrInsertArtistData >>= findOrInsertArtistTree
  where
    findOrInsertArtistData :: MusicBrainz Int
    findOrInsertArtistData = selectValue $
      query [sql| SELECT find_or_insert_artist_data(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) |]
        artist

    findOrInsertArtistTree dataId = selectValue $
      query [sql| SELECT find_or_insert_artist_tree(?) |]
        (Only dataId)

