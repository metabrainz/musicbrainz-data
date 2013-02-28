{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Functions for interacting with 'ReleaseGroup's in the MusicBrainz database.

The majority of operations on releases are common for all core entities, so you
should see the documentation on the 'Release' type and notice all the type class
instances. -}
module MusicBrainz.Data.ReleaseGroup
    ( findByArtist ) where

import Control.Applicative
import Control.Lens (prism)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import qualified Data.Set as Set

import MusicBrainz
import MusicBrainz.Data.Annotation
import MusicBrainz.Data.Create
import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.Merge
import MusicBrainz.Data.Relationship
import MusicBrainz.Data.Relationship.Internal
import MusicBrainz.Data.Revision.Internal
import MusicBrainz.Data.Tree
import MusicBrainz.Data.Update
import MusicBrainz.Edit

import qualified MusicBrainz.Data.Generic as Generic

--------------------------------------------------------------------------------
instance HoldsRelationships ReleaseGroup where
  fetchEndPoints = Generic.fetchEndPoints "release_group"
  reflectRelationshipChange = Generic.reflectRelationshipChange ReleaseGroupRelationship


--------------------------------------------------------------------------------
addSecondaryTypes :: (Functor m, Monad m, MonadIO m)
  => CoreEntity ReleaseGroup -> MusicBrainzT m (CoreEntity ReleaseGroup)
addSecondaryTypes rg = augment <$> query q (Only $ coreRevision rg)
  where
    augment types = rg { coreData = (coreData rg) { releaseGroupSecondaryTypes = Set.fromList $ map fromOnly types } }
    q = [sql| SELECT release_group_secondary_type_id
              FROM release_group_tree_secondary_type
              JOIN release_group_revision USING (release_group_tree_id)
              WHERE revision_id = ? |]

--------------------------------------------------------------------------------
instance FindLatest ReleaseGroup where
  findLatest releaseGroupId = addSecondaryTypes =<< head <$> query q (Only releaseGroupId)
    where q = [sql|
       SELECT release_group_id, revision_id,
         name.name, comment, artist_credit_id, release_group_primary_type_id
      FROM release_group
      JOIN release_group_revision USING (release_group_id)
      JOIN release_group_tree USING (release_group_tree_id)
      JOIN release_group_data USING (release_group_data_id)
      JOIN release_name name ON (release_group_data.name = name.id)
      WHERE release_group_id = ?
        AND revision_id = master_revision_id  |]


--------------------------------------------------------------------------------
instance ViewRevision ReleaseGroup where
  viewRevision revision = addSecondaryTypes =<< head <$> query q (Only revision)
    where q = [sql|
       SELECT release_group_id, revision_id,
        name.name, comment, artist_credit_id, release_group_primary_type_id
      FROM release_group
      JOIN release_group_revision USING (release_group_id)
      JOIN release_group_tree USING (release_group_tree_id)
      JOIN release_group_data USING (release_group_data_id)
      JOIN release_name name ON (release_group_data.name = name.id)
      WHERE revision_id = ? |]


--------------------------------------------------------------------------------
instance Editable ReleaseGroup where
  linkRevisionToEdit = Generic.linkRevisionToEdit "edit_release_group"

  change = prism ReleaseGroupChange extract
    where extract a = case a of ReleaseGroupChange c -> Right c
                                _ -> Left a


--------------------------------------------------------------------------------
instance ViewTree ReleaseGroup where
  viewTree r = ReleaseGroupTree <$> fmap coreData (viewRevision r)
                                <*> viewRelationships r
                                <*> viewAnnotation r


--------------------------------------------------------------------------------
instance Merge ReleaseGroup


 --------------------------------------------------------------------------------
instance Create ReleaseGroup where
  create = Generic.create "release_group"


--------------------------------------------------------------------------------
instance CloneRevision ReleaseGroup where
  cloneRevision = Generic.cloneRevision "release_group"


--------------------------------------------------------------------------------
instance RealiseTree ReleaseGroup where
  realiseTree rg = do
    dataId <- insertRgData (releaseGroupData rg)
    treeId <- insertRgTree (releaseGroupAnnotation rg) dataId
    realiseSecondaryTypes treeId
    Generic.realiseRelationships "release_group" treeId rg
    return treeId
    where
      insertRgData :: (Functor m, MonadIO m) => ReleaseGroup -> MusicBrainzT m Int
      insertRgData data' = selectValue $
        query [sql| SELECT find_or_insert_release_group_data(?, ?, ?, ?) |]
          data'

      insertRgTree annotation dataId = selectValue $
        query [sql| INSERT INTO release_group_tree (release_group_data_id, annotation)
                    VALUES (?, ?)
                    RETURNING release_group_tree_id  |]
          (dataId, annotation)

      realiseSecondaryTypes treeId = executeMany
        [sql| INSERT INTO release_group_tree_secondary_type
                (release_group_tree_id, release_group_secondary_type_id)
              VALUES (?, ?) |] $
        map (\t -> (treeId, t)) (Set.toList $ releaseGroupSecondaryTypes $ releaseGroupData rg)


--------------------------------------------------------------------------------
instance NewEntityRevision ReleaseGroup where
  newEntityRevision revisionId rgId rgTreeId = void $
    execute [sql| INSERT INTO release_group_revision
                    (release_group_id, revision_id, release_group_tree_id)
                  VALUES (?, ?, ?) |]
      (rgId, revisionId, rgTreeId)


--------------------------------------------------------------------------------
instance MasterRevision ReleaseGroup where
  setMasterRevision = Generic.setMasterRevision "release_group"


--------------------------------------------------------------------------------
instance ViewAnnotation ReleaseGroup where
  viewAnnotation = Generic.viewAnnotation "release_group"


--------------------------------------------------------------------------------
instance Update ReleaseGroup


--------------------------------------------------------------------------------
instance ResolveReference ReleaseGroup where
  resolveReference = Generic.resolveMbid "release_group"


--------------------------------------------------------------------------------
instance ResolveReference (Revision ReleaseGroup) where
  resolveReference = Generic.resolveRevision "release_group"


--------------------------------------------------------------------------------
findByArtist :: MonadIO m => Ref Artist -> MusicBrainzT m [CoreEntity ReleaseGroup]
findByArtist artistId = query q (Only artistId)
  where
    q = [sql| SELECT release_group_id, revision_id,
                 name.name, comment, artist_credit_id, release_group_primary_type_id
              FROM (
                SELECT DISTINCT release_group_id, rgs.revision_id,
                    rgs.name, rgs.comment, rgs.artist_credit_id,
                    release_group_primary_type_id,
                  array(
                    SELECT name
                    FROM release_group_secondary_type
                    JOIN release_group_tree_secondary_type
                      ON (release_group_secondary_type_id = release_group_secondary_type.id)
                    WHERE release_group_tree_id = rgs.release_group_tree_id
                    ORDER BY musicbrainz_collate(name) ASC
                  ) secondary_types,
                  first_value(date_year) OVER w AS first_release_date_year,
                  first_value(date_month) OVER w AS first_release_date_month,
                  first_value(date_day) OVER w AS first_release_date_day
                FROM (
                  SELECT DISTINCT release_group_id, revision_id,
                    release_group_data.name, comment, artist_credit_id,
                    release_group_primary_type_id, release_group_tree_id
                  FROM release_group
                  JOIN release_group_revision USING (release_group_id)
                  JOIN release_group_tree USING (release_group_tree_id)
                  JOIN release_group_data USING (release_group_data_id)
                  JOIN artist_credit_name USING (artist_credit_id)
                  WHERE artist_credit_name.artist_id = ?
                    AND revision_id = master_revision_id
                ) rgs
                JOIN release_tree USING (release_group_id)
                JOIN release_data USING (release_data_id)
                JOIN release_revision USING (release_tree_id)
                JOIN release USING (release_id)
                WHERE release.master_revision_id = release_revision.revision_id
                WINDOW w AS (
                  ORDER BY date_year ASC NULLS LAST,
                    date_month ASC NULLS LAST,
                    date_day ASC NULLS LAST
                )
              ) rgs_type
              JOIN release_name name ON (name.id = rgs_type.name)
              ORDER BY
                release_group_primary_type_id,
                secondary_types,
                first_release_date_year NULLS LAST,
                first_release_date_month NULLS LAST,
                first_release_date_day NULLS LAST,
                musicbrainz_collate(name.name)
            |]
