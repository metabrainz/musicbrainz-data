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
import Data.Traversable (traverse)
import Database.PostgreSQL.Simple (Only(..), In(..))
import Database.PostgreSQL.Simple.SqlQQ

import MusicBrainz
import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.Revision
import MusicBrainz.Edit
import MusicBrainz.Merge

import qualified MusicBrainz.Data.Generic.Create as GenericCreate

--------------------------------------------------------------------------------
instance Editable Artist where
  includeRevision editId revisionId = void $ execute
    [sql| INSERT INTO edit_artist (edit_id, revision_id) VALUES (?, ?) |]
      (editId, revisionId)

  mergeRevisionUpstream new = do
    newVer <- viewRevision new
    let artistId = coreMbid newVer

    current' <- findLatest artistId
    case current' of
      Nothing -> error "Unable to merge: nothing to merge into"
      Just current -> do
        ancestor' <- mergeBase new (coreRevision current) >>= traverse viewRevision
        case ancestor' of
          Nothing -> error "Unable to merge: no common ancestor"
          Just ancestor ->
            case runMerge (coreData newVer) (coreData current) (coreData ancestor) merge of
              Nothing -> error "Unable to merge: conflict"
              Just merged -> do
                let editorId = EditorRef 1

                treeId <- artistTree merged
                revisionId <- newRevision editorId >>=
                              newArtistRevision (coreRevision current) treeId
                addChild revisionId new
                addChild revisionId (coreRevision current)
                linkRevision artistId revisionId


--------------------------------------------------------------------------------
instance FindLatest Artist where
  findLatest artistId = listToMaybe <$> query q (Only artistId)
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
{-| View an artist at an exact 'Revision'. -}
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
{-| Find references to the parent revisions of a given revision. -}
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
    , GenericCreate.newEntityRevision = newArtistRevision'
    , GenericCreate.linkRevision = linkRevision
    }
  where
    newArtistRevision' artistId artistTreeId revisionId = selectValue $
      query [sql| INSERT INTO artist_revision (artist_id, revision_id, artist_tree_id)
                  VALUES (?, ?, ?) RETURNING revision_id |]
        (artistId, revisionId, artistTreeId)


linkRevision :: MBID Artist -> Ref (Revision Artist) -> MusicBrainz ()
linkRevision artistId revisionId = void $
  execute [sql| UPDATE artist SET master_revision_id = ?
                WHERE artist_id = ? |] (revisionId, artistId)


--------------------------------------------------------------------------------
{-| Update the information about an artist, yielding a new revision. -}
update :: Ref Editor -> Ref (Revision Artist) -> Artist
       -> MusicBrainz (Ref (Revision Artist))
update editor baseRev artist = do
  newTree <- artistTree artist
  revisionId <- newRevision editor >>= newArtistRevision baseRev newTree
  addChild revisionId baseRev
  return revisionId


--------------------------------------------------------------------------------
newArtistRevision :: Ref (Revision Artist)
                  -> Ref (Tree Artist)
                  -> Ref (Revision Artist)
                  -> MusicBrainz (Ref (Revision Artist))
newArtistRevision parentRevision artistTreeId revisionId = selectValue $
  query [sql| INSERT INTO artist_revision (artist_id, revision_id, artist_tree_id)
              VALUES ( (SELECT artist_id FROM artist_revision WHERE revision_id = ?)
                     , ?, ?) RETURNING revision_id |]
    (parentRevision, revisionId, artistTreeId)


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


--------------------------------------------------------------------------------
{-| Attempt to resolve a the revision which 2 revisions forked from. -}
mergeBase :: Ref (Revision Artist) -> Ref (Revision Artist)
          -> MusicBrainz (Maybe (Ref (Revision Artist)))
mergeBase a b = selectValue $ query
  [sql| WITH RECURSIVE revision_path (revision_id, parent_revision_id, distance)
        AS (
          SELECT revision_id, parent_revision_id, 1
          FROM revision_parent
          WHERE revision_id IN ?

          UNION

          SELECT
            revision_path.revision_id, revision_parent.parent_revision_id,
            distance + 1
          FROM revision_parent
          JOIN revision_path
            ON (revision_parent.revision_id = revision_path.parent_revision_id)
        )
        SELECT parent_revision_id
        FROM revision_path a
        JOIN revision_path b USING (parent_revision_id)
        ORDER BY a.distance, b.distance
        LIMIT 1 |] (Only $ In [a, b])
