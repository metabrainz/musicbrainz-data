{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions for interacting with MusicBrainz artists in the database. -}
module MusicBrainz.Data.Artist
    ( -- * Working with revisions
      viewRevision
    , viewTree
    , viewAliases
    , viewIpiCodes
    , viewAnnotation

    -- * Artist MBID handling
    , resolveMbid

      -- * Editing artists
    , update
    , MusicBrainz.Data.Artist.merge
    ) where

import Prelude hiding (mapM_)

import Control.Applicative
import Control.Lens hiding (by, query)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (mapM_, forM_)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Only(..), (:.)(..))
import Database.PostgreSQL.Simple.SqlQQ

import qualified Data.Set as Set

import MusicBrainz
import MusicBrainz.Data.Create
import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.Relationship
import MusicBrainz.Data.Revision
import MusicBrainz.Edit
import MusicBrainz.Lens
import MusicBrainz.Merge

import qualified MusicBrainz.Data.Generic.Create as GenericCreate
import qualified MusicBrainz.Merge as Merge

--------------------------------------------------------------------------------
instance HoldsRelationships Artist where
  fetchEndPoints r ToArtist = do
    rels <- query [sql|
      SELECT target_id, relationship_id
      FROM artist_revision
      JOIN artist_tree USING (artist_tree_id)
      JOIN l_artist_artist ON (source_id = artist_tree_id)
      WHERE revision_id = ?
    |] (Only r)
    return $ map constructPartialRel rels
    where
      constructPartialRel (targetId, relationshipId) =
        (ArtistRelationship targetId, relationshipId)


--------------------------------------------------------------------------------
{-| View all data about a specific version of an 'Artist'. -}
viewTree :: (Applicative m, MonadIO m)
  => Ref (Revision Artist) -> MusicBrainzT m (Tree Artist)
viewTree r = ArtistTree <$> fmap coreData (viewRevision r)
                        <*> viewRelationships r
                        <*> viewAliases r
                        <*> viewIpiCodes r
                        <*> viewAnnotation r


--------------------------------------------------------------------------------
{-| View all aliases for a specific revision of an 'Artist'. -}
viewAliases :: (Functor m, MonadIO m)
  => Ref (Revision Artist) -> MusicBrainzT m (Set.Set Alias)
viewAliases r = Set.fromList <$> query
  [sql| SELECT name.name, sort_name.name,
          begin_date_year, begin_date_month, begin_date_day,
          end_date_year, end_date_month, end_date_day,
          ended, artist_alias_type_id, locale
        FROM artist_alias
        JOIN artist_name name ON (artist_alias.name = name.id)
        JOIN artist_name sort_name ON (artist_alias.sort_name = sort_name.id)
        JOIN artist_tree USING (artist_tree_id)
        JOIN artist_revision USING (artist_tree_id)
        WHERE revision_id = ? |] (Only r)


--------------------------------------------------------------------------------
{-| View all IPI codes for a specific revision of an 'Artist'. -}
viewIpiCodes :: (Functor m, MonadIO m)
  => Ref (Revision Artist) -> MusicBrainzT m (Set.Set IPI)
viewIpiCodes r = Set.fromList <$> query
  [sql| SELECT ipi
        FROM artist_ipi
        JOIN artist_tree USING (artist_tree_id)
        JOIN artist_revision USING (artist_tree_id)
        WHERE revision_id = ? |] (Only r)


--------------------------------------------------------------------------------
{-| View the annotation for a specific revision of an 'Artist'. -}
viewAnnotation :: (Functor m, MonadIO m)
  => Ref (Revision Artist) -> MusicBrainzT m Text
viewAnnotation r = fromOnly . head <$> query
  [sql| SELECT annotation
        FROM artist_tree
        JOIN artist_revision USING (artist_tree_id)
        WHERE revision_id = ? |] (Only r)


--------------------------------------------------------------------------------
instance Editable Artist where
  linkRevisionToEdit editId revisionId = void $ execute
    [sql| INSERT INTO edit_artist (edit_id, revision_id) VALUES (?, ?) |]
      (editId, revisionId)

  mergeRevisionUpstream new = do
    newVer <- viewRevision new
    let artistId = coreRef newVer

    current <- findLatest artistId
    ancestor' <- mergeBase new (coreRevision current) >>= traverse viewRevision
    case ancestor' of
      Nothing -> error "Unable to merge: no common ancestor"
      Just ancestor -> do
        newTree <- viewTree new
        currentTree <- viewTree (coreRevision current)
        ancestorTree <- viewTree (coreRevision ancestor)

        case runMerge newTree currentTree ancestorTree Merge.merge of
          Nothing -> error "Unable to merge: conflict"
          Just merged -> do
            editorId <- selectValue $ query
              [sql| SELECT editor_id FROM revision WHERE revision_id = ? |]
                (Only $ coreRevision current)

            treeId <- artistTree merged
            revisionId <- newRevision editorId >>=
                          newArtistRevision (coreRevision current) treeId
            addChild revisionId new
            addChild revisionId (coreRevision current)
            linkRevision artistId revisionId


--------------------------------------------------------------------------------
instance FindLatest Artist where
  findLatest artistId = head <$> query q (Only artistId)
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
viewRevision :: (Functor m, MonadIO m) => Ref (Revision Artist) -> MusicBrainzT m (CoreEntity Artist)
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
instance Create Artist where
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


--------------------------------------------------------------------------------
linkRevision :: (Functor m, MonadIO m) => Ref Artist -> Ref (Revision Artist) -> MusicBrainzT m ()
linkRevision artistId revisionId = void $
  execute [sql| UPDATE artist SET master_revision_id = ?
                WHERE artist_id = ? |] (revisionId, artistId)


--------------------------------------------------------------------------------
{-| Update the information about an artist, yielding a new revision. -}
update :: Ref Editor -> Ref (Revision Artist) -> Tree Artist
       -> EditM (Ref (Revision Artist))
update editor baseRev artist = do
  -- Create the new revision for this artist
  revisionId <- runUpdate artist baseRev

  -- Reflect relationship changes against other entities
  oldRelationships <- viewRelationships baseRev
  let additions = (artistRelationships artist) `Set.difference` oldRelationships
  let deletions = oldRelationships `Set.difference` (artistRelationships artist)

  when (not $ Set.null additions && Set.null deletions) $ do
    self <- viewRevision baseRev

    forM_ additions $
      reflectRelationshipChange self Set.insert

    forM_ deletions $
      reflectRelationshipChange self Set.delete

  return revisionId

  where
    runUpdate tree base = do
      treeId <- artistTree tree
      revisionId <- newRevision editor >>= newArtistRevision base treeId
      includeRevision revisionId
      addChild revisionId base
      return revisionId

    reflectRelationshipChange endpoint f (ArtistRelationship targetId rel) = do
      let returnRelationship = ArtistRelationship (coreRef endpoint) rel
      target <- findLatest targetId
      targetTree <- over relationships (f returnRelationship) <$> viewTree (coreRevision target)
      runUpdate targetTree (coreRevision target)


--------------------------------------------------------------------------------
{-| Merge an artist into another artist. -}
merge :: Ref Editor -> Ref (Revision Artist) -> Ref Artist
      -> EditM (Ref (Revision Artist))
merge editor baseRev targetId = do
  -- Find the latest revision to merge into
  latestTarget <- findLatest targetId
  mergeInto <- cloneRevision latestTarget

  -- Link this revision to both the old tree and the latest version,
  -- and include it in the edit.
  includeRevision mergeInto
  addChild mergeInto baseRev
  addChild mergeInto (coreRevision latestTarget)

  return mergeInto

  where
    cloneRevision a = do
      revId <- newRevision editor
      selectValue $
        query [sql|
          INSERT INTO artist_revision (artist_id, revision_id, artist_tree_id)
          VALUES (?, ?, (SELECT artist_tree_id FROM artist_revision WHERE revision_id = ?))
          RETURNING revision_id
        |] (coreRef a, revId, coreRevision a)


--------------------------------------------------------------------------------
newArtistRevision :: (Functor m, MonadIO m)
                  => Ref (Revision Artist)
                  -> Ref (Tree Artist)
                  -> Ref (Revision Artist)
                  -> MusicBrainzT m (Ref (Revision Artist))
newArtistRevision parentRevision artistTreeId revisionId = selectValue $
  query [sql| INSERT INTO artist_revision (artist_id, revision_id, artist_tree_id)
              VALUES ( (SELECT artist_id FROM artist_revision WHERE revision_id = ?)
                     , ?, ?) RETURNING revision_id |]
    (parentRevision, revisionId, artistTreeId)


--------------------------------------------------------------------------------
artistTree :: (Functor m, Monad m, MonadIO m) => Tree Artist -> MusicBrainzT m (Ref (Tree Artist))
artistTree artist = do
  dataId <- insertArtistData (artistData artist)
  treeId <- insertArtistTree (artistAnnotation artist) dataId

  mapM_ (addRelationship treeId) $ artistRelationships artist

  forM_ (Set.toList $ artistAliases artist) $ \alias -> do
    execute [sql|
      INSERT INTO artist_alias (artist_tree_id, name, sort_name,
        begin_date_year, begin_date_month, begin_date_day,
        end_date_year, end_date_month, end_date_day,
        ended, artist_alias_type_id, locale)
      VALUES (?, (SELECT find_or_insert_artist_name(?)),
        (SELECT find_or_insert_artist_name(?)), ?, ?, ?, ?, ?, ?, ?, ?, ?) |]
      (Only treeId :. alias)

  executeMany [sql| INSERT INTO artist_ipi (artist_tree_id, ipi) VALUES (?, ?) |]
    $ map (Only treeId :.) (Set.toList $ artistIpiCodes artist)

  return treeId
  where
    insertArtistData :: (Functor m, MonadIO m) => Artist -> MusicBrainzT m Int
    insertArtistData data' = selectValue $
      query [sql| SELECT find_or_insert_artist_data(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) |]
        data'

    insertArtistTree annotation dataId = selectValue $
      query [sql| INSERT INTO artist_tree (artist_data_id, annotation)
                  VALUES (?, ?)
                  RETURNING artist_tree_id  |]
        (dataId, annotation)

    addRelationship treeId (ArtistRelationship targetId relInfo) = do
      relationshipId <- selectValue $ query [sql|
        INSERT INTO relationship (relationship_type_id,
          begin_date_year, begin_date_month, begin_date_day,
          end_date_year, end_date_month, end_date_day,
          ended)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?) RETURNING relationship_id |] relInfo
      execute [sql| INSERT INTO l_artist_artist (source_id, target_id, relationship_id) VALUES (?, ?, ?) |]
        (treeId, targetId, relationshipId :: Int)


--------------------------------------------------------------------------------
{-| Attempt to resolve an 'MBID Artist' to a specific 'Artist' 'Ref'. This
will follow merges to find the correct artist this MBID now points to. -}
resolveMbid :: (Functor m, MonadIO m) => MBID Artist
  -> MusicBrainzT m (Maybe (Ref Artist))
resolveMbid entityMbid =
  listToMaybe . map fromOnly <$> query
    [sql|
      WITH RECURSIVE path (revision_id, artist_id, child_revision_id, created_at, is_master_revision_id)
      AS (
        SELECT
          artist_revision.revision_id,
          artist_revision.artist_id,
          revision_parent.revision_id AS child_revision_id,
          created_at,
          TRUE as is_master_revision_id
        FROM artist_revision
        JOIN artist USING (artist_id)
        JOIN revision USING (revision_id)
        LEFT JOIN revision_parent ON (revision_parent.parent_revision_id = revision.revision_id)
        WHERE artist_id = ? AND master_revision_id = artist_revision.revision_id

        UNION

        SELECT
          artist_revision.revision_id,
          artist_revision.artist_id,
          revision_parent.revision_id,
          revision.created_at,
          master_revision_id = artist_revision.revision_id AS is_master_revision_id
        FROM path
        JOIN artist_revision ON (path.child_revision_id = artist_revision.revision_id)
        JOIN revision ON (revision.revision_id = artist_revision.revision_id)
        JOIN artist ON (artist.artist_id = artist_revision.artist_id)
        LEFT JOIN revision_parent ON (revision_parent.parent_revision_id = artist_revision.revision_id)
      )
      SELECT artist_id
      FROM path
      WHERE is_master_revision_id
      ORDER BY created_at, revision_id DESC
      LIMIT 1
    |]
      (Only entityMbid)
