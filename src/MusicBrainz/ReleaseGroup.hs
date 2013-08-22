{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module MusicBrainz.ReleaseGroup where

import Control.Applicative
import Control.Lens
import Control.Monad ((<=<), void)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (listToMaybe)
import Data.Monoid (mempty) -- See FIXME 1
import Data.Set (Set)
import Data.String (fromString)
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.ToRow (ToRow(..))

import MusicBrainz.Merge
import MusicBrainz.Monad
import MusicBrainz.Annotation
import MusicBrainz.Artist
import MusicBrainz.ArtistCredit (ArtistCredit)
import MusicBrainz.Class.Add
import MusicBrainz.Class.Create
import MusicBrainz.Class.FindLatest
import MusicBrainz.Class.MasterRevision
import MusicBrainz.Class.NewEntityRevision
import MusicBrainz.Class.RealiseTree
import MusicBrainz.Class.ResolveReference
import MusicBrainz.Class.RootTable
import MusicBrainz.Class.Update
import MusicBrainz.Class.ViewRevision
import MusicBrainz.Edit (Editable(..))
import MusicBrainz.Entity
import MusicBrainz.MBID (MBID)
import MusicBrainz.Ref (Ref, Referenceable(..), reference, dereference)
import MusicBrainz.Relationship
import MusicBrainz.Relationship.Internal (HoldsRelationships(..), viewRelationships)
import MusicBrainz.Revision (Revision)
import MusicBrainz.Revision.Internal (CloneRevision(..))
import MusicBrainz.Tree

import qualified Data.Set as Set

import {-# SOURCE #-} qualified MusicBrainz.Generic as Generic

--------------------------------------------------------------------------------
{-| A release group is an abstract MusicBrainz concept which groups multiple
'Release's logically together. For example, a release group might contain the
various different formats of albums, such as the vinyl release and the CD
release. -}
data ReleaseGroup = ReleaseGroup
    { releaseGroupName :: !Text
    , releaseGroupComment :: !Text
    , releaseGroupArtistCredit :: !(Ref ArtistCredit)
    , releaseGroupPrimaryType :: !(Maybe (Ref (ReleaseGroupType Primary)))
    , releaseGroupSecondaryTypes :: !(Set (Ref (ReleaseGroupType Secondary)))
    }
  deriving (Eq, Show)

instance Referenceable ReleaseGroup where
  type RefSpec ReleaseGroup = MBID ReleaseGroup

instance FromField (Ref ReleaseGroup) where
  fromField f v = view reference <$> fromField f v

instance FromRow ReleaseGroup where
  fromRow = ReleaseGroup <$> field <*> field <*> field <*> field
                         <*> pure mempty -- FIXME 1

instance ToField (Ref ReleaseGroup) where
  toField = toField . dereference

instance ToRow ReleaseGroup where
  toRow ReleaseGroup{..} = [ toField releaseGroupName
                           , toField releaseGroupComment
                           , toField releaseGroupArtistCredit
                           , toField releaseGroupPrimaryType
                           ]

instance HasTree ReleaseGroup where
  data Tree ReleaseGroup =
    ReleaseGroupTree { releaseGroupData :: !ReleaseGroup
                     , releaseGroupRelationships :: !(Set LinkedRelationship)
                     , releaseGroupAnnotation :: !Text
                     }

  treeData ReleaseGroupTree{..} = releaseGroupData

instance TreeAnnotation ReleaseGroup where
  annotation f releaseGroup =
    f (releaseGroupAnnotation releaseGroup) <&> \b -> releaseGroup { releaseGroupAnnotation = b }

instance TreeRelationships ReleaseGroup where
  relationships f releaseGroup =
    f (releaseGroupRelationships releaseGroup) <&> \b -> releaseGroup { releaseGroupRelationships = b }

instance Mergeable (Tree ReleaseGroup) where
  type MergeRender (Tree ReleaseGroup) mo =
    ( Render (Maybe (Ref (ReleaseGroupType Primary))) mo
    , Render (Set.Set LinkedRelationship) mo
    , Render (Set.Set (Ref (ReleaseGroupType Secondary))) mo
    , Render (Ref ArtistCredit) mo
    , Render Text mo
    )

  merge =
    ReleaseGroupTree <$> releaseGroupData `mergedVia` mergeReleaseGroupData
                     <*> releaseGroupRelationships `mergedVia` merge
                     <*> releaseGroupAnnotation `mergedVia` mergeEq
    where
      mergeReleaseGroupData =
        ReleaseGroup
              <$> releaseGroupName `mergedVia` mergeEq
              <*> releaseGroupComment `mergedVia` mergeEq
              <*> releaseGroupArtistCredit `mergedVia` mergeEq
              <*> releaseGroupPrimaryType `mergedVia` mergeEq
              <*> releaseGroupSecondaryTypes `mergedVia` merge

instance CloneRevision ReleaseGroup

instance Create ReleaseGroup

instance MasterRevision ReleaseGroup

instance ResolveReference (Revision ReleaseGroup)

instance ResolveReference ReleaseGroup

instance Update ReleaseGroup

instance ViewAnnotation ReleaseGroup

instance RootTable ReleaseGroup where
  rootTable = Tagged "release_group"

instance HoldsRelationships ReleaseGroup where
  reflectRelationshipChange = Generic.reflectRelationshipChange ReleaseGroupRelationship

instance FindLatest ReleaseGroup where
  findLatest = traverse addSecondaryTypes <=< Generic.findLatest
    [sql|
      SELECT release_group_id, revision_id,
         name.name, comment, artist_credit_id, release_group_primary_type_id
      FROM release_group
      JOIN release_group_revision USING (release_group_id)
      JOIN release_group_tree USING (release_group_tree_id)
      JOIN release_group_data USING (release_group_data_id)
      JOIN release_name name ON (release_group_data.name = name.id)
      WHERE release_group_id IN ?
        AND revision_id = master_revision_id  |]

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

instance Editable ReleaseGroup

instance ViewTree ReleaseGroup where
  viewTree r = ReleaseGroupTree <$> fmap coreData (viewRevision r)
                                <*> viewRelationships r
                                <*> viewAnnotation r

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

      insertRgTree annotationBody dataId = selectValue $
        query [sql| INSERT INTO release_group_tree (release_group_data_id, annotation)
                    VALUES (?, ?)
                    RETURNING release_group_tree_id  |]
          (dataId, annotationBody)

      realiseSecondaryTypes treeId = executeMany
        [sql| INSERT INTO release_group_tree_secondary_type
                (release_group_tree_id, release_group_secondary_type_id)
              VALUES (?, ?) |] $
        map (\t -> (treeId, t)) (Set.toList $ releaseGroupSecondaryTypes $ releaseGroupData rg)

instance NewEntityRevision ReleaseGroup where
  newEntityRevision revisionId rgId rgTreeId = void $
    execute [sql| INSERT INTO release_group_revision
                    (release_group_id, revision_id, release_group_tree_id)
                  VALUES (?, ?, ?) |]
      (rgId, revisionId, rgTreeId)


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
{-| A type index for 'ReleaseGroupType' indicating that this 'ReleaseGroupType'
is primary and can only occur once. -}
data Primary

{-| A type index for 'ReleaseGroupType' indicating that this 'ReleaseGroupType'
is secondary and can only occur multiple times. -}
data Secondary


--------------------------------------------------------------------------------
{-| A release group type indicates the various types a release group can be.
For example, one release group type combination might be 'Album + Remix' to
indicate a remix album.

The parameter to 'ReleaseGroupType' indicates whether the release group
type is primary or secondary. -}
newtype ReleaseGroupType a = ReleaseGroupType { releaseGroupTypeName :: Text }
  deriving (Eq, Show)

instance Referenceable (ReleaseGroupType a) where
  type RefSpec (ReleaseGroupType a) = Int

instance FromField (Ref (ReleaseGroupType a)) where
  fromField f v = view reference <$> fromField f v

instance FromRow (ReleaseGroupType a) where
  fromRow = ReleaseGroupType <$> field

instance ToField (Ref (ReleaseGroupType a)) where
  toField = toField . dereference

instance ToRow (ReleaseGroupType a) where
  toRow ReleaseGroupType{..} = [ toField releaseGroupTypeName
                               ]

instance Add (ReleaseGroupType Primary) where
  add = addRgType "release_group_primary_type"

instance ResolveReference (ReleaseGroupType Primary) where
  resolveReference = resolveRgType "release_group_primary_type"

instance Add (ReleaseGroupType Secondary) where
  add = addRgType "release_group_secondary_type"

instance ResolveReference (ReleaseGroupType Secondary) where
  resolveReference = resolveRgType "release_group_secondary_type"


--------------------------------------------------------------------------------
addRgType :: (Functor m, MonadIO m)
  => String -> ReleaseGroupType a -> MusicBrainzT m (Entity (ReleaseGroupType a))
addRgType rgTable rgType = head <$> query insertSql rgType
  where insertSql =
          fromString $ unlines
            [ "INSERT INTO " ++ rgTable ++ " (name)"
            , " VALUES (?) RETURNING id, name"
            ]

resolveRgType :: (Functor m, MonadIO m)
  => String -> RefSpec (ReleaseGroupType a) -> MusicBrainzT m (Maybe (Ref (ReleaseGroupType a)))
resolveRgType rgTable rgTypeId = listToMaybe . map fromOnly <$> query selectSql (Only rgTypeId)
  where selectSql = fromString $ "SELECT id FROM " ++ rgTable ++ " WHERE id = ?"


--------------------------------------------------------------------------------
findByArtist :: (Functor m, MonadIO m) => Ref Artist -> MusicBrainzT m [CoreEntity ReleaseGroup]
findByArtist artistId = mapM addSecondaryTypes =<< query q (Only artistId)
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
                LEFT JOIN release_tree USING (release_group_id)
                LEFT JOIN release_data USING (release_data_id)
                LEFT JOIN release_revision USING (release_tree_id)
                LEFT JOIN release USING (release_id)
                WHERE release.master_revision_id IS NULL
                  OR release.master_revision_id = release_revision.revision_id
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
