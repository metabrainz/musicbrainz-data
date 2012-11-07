{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions for interacting with MusicBrainz artists in the database. -}
module MusicBrainz.Data.Artist
    ( -- * Working with revisions
      viewRevision
    , revisionParents
    , viewTree
    , viewRelationships
    , viewAliases
    , viewIpiCodes

      -- * Editing artists
    , create
    , update
    ) where

import Prelude hiding (mapM_)

import Control.Applicative
import Control.Lens hiding (query)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (mapM_, forM_)
import Data.Proxy
import Database.PostgreSQL.Simple (Only(..), (:.)(..))
import Database.PostgreSQL.Simple.SqlQQ

import qualified Data.Set as Set

import MusicBrainz
import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.Revision
import MusicBrainz.Edit
import MusicBrainz.Lens
import MusicBrainz.Merge

import qualified MusicBrainz.Data.Generic.Create as GenericCreate

--------------------------------------------------------------------------------
viewTree :: (Applicative m, MonadIO m) => Ref (Revision Artist) -> MusicBrainzT m (Tree Artist)
viewTree r = ArtistTree <$> fmap coreData (viewRevision r)
                        <*> viewRelationships r
                        <*> viewAliases r
                        <*> viewIpiCodes r


--------------------------------------------------------------------------------
viewRelationships :: (Functor m, MonadIO m) => Ref (Revision Artist) -> MusicBrainzT m (Set.Set Relationship)
viewRelationships r = Set.fromList . concat <$> sequence [ toArtist ]
  where
    toArtist =
      let detag x = proxy x (Proxy :: Proxy Artist)
      in map detag <$> query [sql|
          SELECT target_id
          FROM artist_revision
          JOIN artist_tree USING (artist_tree_id)
          JOIN l_artist_artist ON (source_id = artist_tree_id)
          WHERE revision_id = ?
        |] (Only r)


--------------------------------------------------------------------------------
viewAliases :: (Functor m, MonadIO m) => Ref (Revision Artist) -> MusicBrainzT m (Set.Set Alias)
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
viewIpiCodes :: (Functor m, MonadIO m) => Ref (Revision Artist) -> MusicBrainzT m (Set.Set IPI)
viewIpiCodes r = Set.fromList <$> query
  [sql| SELECT ipi
        FROM artist_ipi
        JOIN artist_tree USING (artist_tree_id)
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

        case runMerge newTree currentTree ancestorTree merge of
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
{-| Find references to the parent revisions of a given revision. -}
revisionParents :: (Functor m, MonadIO m) => Ref (Revision Artist) -> MusicBrainzT m (Set.Set (Ref (Revision Artist)))
revisionParents artistRev =
  Set.fromList . map fromOnly <$> query q (Only artistRev)
  where q = [sql| SELECT parent_revision_id FROM revision_parent
                  WHERE revision_id = ? |]


--------------------------------------------------------------------------------
{-| Create an entirely new artist, returning the final 'CoreEntity' as it is
in the database. -}
create :: (Functor m, MonadIO m) => Ref Editor -> Tree Artist -> MusicBrainzT m (CoreEntity Artist)
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
    let returnRelationship = ArtistRelationship (coreRef self)

    forM_ additions $
      reflectRelationshipChange (Set.insert returnRelationship)

    forM_ deletions $
      reflectRelationshipChange (Set.delete returnRelationship)

  return revisionId

  where
    runUpdate tree base = do
      treeId <- artistTree tree
      revisionId <- newRevision editor >>= newArtistRevision base treeId
      includeRevision revisionId
      addChild revisionId base
      return revisionId

    reflectRelationshipChange f (ArtistRelationship targetId) = do
      target <- findLatest targetId
      targetTree <- over relationships f <$> viewTree (coreRevision target)
      runUpdate targetTree (coreRevision target)


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
  treeId <- insertArtistTree dataId

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

  executeMany [sql| INSERT INTO artist_ipi (artist_tree_id, ipi_code) VALUES (?, ?) |]
    $ map (Only treeId :.) (Set.toList $ artistIpiCodes artist)

  return treeId
  where
    insertArtistData :: (Functor m, MonadIO m) => Artist -> MusicBrainzT m Int
    insertArtistData data' = selectValue $
      query [sql| SELECT find_or_insert_artist_data(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) |]
        data'

    insertArtistTree dataId = selectValue $
      query [sql| INSERT INTO artist_tree (artist_data_id)
                  VALUES (?)
                  RETURNING artist_tree_id  |]
        (Only dataId)

    addRelationship treeId (ArtistRelationship targetId) =
      execute [sql| INSERT INTO l_artist_artist (source_id, target_id) VALUES (?, ?) |]
        (treeId, targetId)

