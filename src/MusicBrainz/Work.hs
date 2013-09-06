{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
module MusicBrainz.Work where

import Control.Applicative
import Control.Arrow
import Control.Lens hiding ((.>))
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (forM_)
import Data.Function
import Data.List (groupBy)
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import Database.PostgreSQL.Simple (In(..), Only(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.ToRow (ToRow(..))

import MusicBrainz.Merge
import MusicBrainz.Monad
import MusicBrainz.Alias
import MusicBrainz.Annotation
import MusicBrainz.Artist
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
import MusicBrainz.ISWC hiding (iswc)
import MusicBrainz.Language (Language)
import MusicBrainz.MBID (MBID)
import MusicBrainz.Ref (Ref, Referenceable(..), reference, dereference)
import MusicBrainz.Relationship
import MusicBrainz.Relationship.Internal (HoldsRelationships(..), viewRelationships)
import MusicBrainz.Revision (Revision)
import MusicBrainz.Revision.Internal (CloneRevision(..))
import MusicBrainz.Tree

import {-# SOURCE #-} qualified MusicBrainz.Generic as Generic

import qualified Data.Map as Map
import qualified Data.Set as Set

--------------------------------------------------------------------------------
data Work = Work
    { workName :: !Text
    , workComment :: !Text
    , workType :: !(Maybe (Ref WorkType))
    , workLanguage :: !(Maybe (Ref Language))
    }
  deriving (Eq, Show)

instance Referenceable Work where
  type RefSpec Work = MBID Work

instance FromField (Ref Work) where
  fromField f v = view reference <$> fromField f v

instance FromRow Work where
  fromRow = Work <$> field <*> field <*> field <*> field

instance ToField (Ref Work) where
  toField = toField . dereference

instance ToRow Work where
  toRow Work{..} = [ toField workName
                   , toField workComment
                   , toField workType
                   , toField workLanguage
                   ]

instance HasTree Work where
  data Tree Work =
    WorkTree { workData :: !Work
             , workRelationships :: !(Set LinkedRelationship)
             , workAliases :: !(Set (Alias Work))
             , workAnnotation :: !Text
             , workIswcs :: !(Set ISWC)
             }

  treeData WorkTree{..} = workData

deriving instance Eq (Tree Work)
deriving instance Show (Tree Work)

instance TreeAliases Work where
  aliases f work =
    f (workAliases work) <&> \b -> work { workAliases = b }

instance TreeAnnotation Work where
  annotation f work =
    f (workAnnotation work) <&> \b -> work { workAnnotation = b }

instance TreeRelationships Work where
  relationships f work =
    f (workRelationships work) <&> \b -> work { workRelationships = b }

instance Mergeable (Tree Work) where
  type MergeRender (Tree Work) mo =
    ( Render (Maybe (Ref Language)) mo
    , Render (Maybe (Ref WorkType)) mo
    , Render (Set.Set (Alias Work)) mo
    , Render (Set.Set ISWC) mo
    , Render (Set.Set LinkedRelationship) mo
    , Render Text mo )

  merge =
    WorkTree <$> workData `mergedVia` mergeWorkData
             <*> "Relationships" .> workRelationships `mergedVia` merge
             <*> "Aliases" .> workAliases `mergedVia` merge
             <*> "Annotation" .> workAnnotation `mergedVia` mergeEq
             <*> "ISWCs" .> workIswcs `mergedVia` merge
    where
      mergeWorkData =
        Work
              <$> "Name" .> workName `mergedVia` mergeEq
              <*> "Comment" .> workComment `mergedVia` mergeEq
              <*> "Type" .> workType `mergedVia` mergeEq
              <*> "Language" .> workLanguage `mergedVia` mergeEq

instance CloneRevision Work

instance Create Work

instance MasterRevision Work

instance NewEntityRevision Work

instance ResolveReference (Revision Work)

instance ResolveReference Work

instance Update Work

instance ViewAliases Work

instance ViewAnnotation Work

instance RootTable Work where
  rootTable = Tagged "work"

instance HoldsRelationships Work where
  reflectRelationshipChange = Generic.reflectRelationshipChange WorkRelationship

instance FindLatest Work where
  findLatest = Generic.findLatest
    [sql|
      SELECT work_id, revision_id,
        name.name, comment, work_type_id, language_id
      FROM work
      JOIN work_revision USING (work_id)
      JOIN work_tree USING (work_tree_id)
      JOIN work_data USING (work_data_id)
      JOIN work_name name ON (work_data.name = name.id)
      WHERE work_id IN ?
        AND revision_id = master_revision_id  |]

instance RealiseTree Work where
  realiseTree work = do
    dataId <- insertWorkData (workData work)
    treeId <- insertWorkTree (workAnnotation work) dataId
    Generic.realiseAliases "work" treeId work
    Generic.realiseRelationships "work" treeId work
    realiseIswcs treeId
    return treeId
    where
      insertWorkData :: (Functor m, MonadIO m) => Work -> MusicBrainzT m Int
      insertWorkData data' = selectValue $
        query [sql| SELECT find_or_insert_work_data(?, ?, ?, ?) |]
          data'

      insertWorkTree annotationBody dataId = selectValue $
        query [sql| INSERT INTO work_tree (work_data_id, annotation)
                    VALUES (?, ?)
                    RETURNING work_tree_id  |]
          (dataId, annotationBody)

      realiseIswcs treeId = forM_ (workIswcs work) $ \iswc ->
        execute q (treeId, iswc)
        where q = [sql| INSERT INTO iswc (work_tree_id, iswc) VALUES (?, ?) |]

instance ViewRevision Work where
  viewRevision revisionId = head <$> query q (Only revisionId)
    where q = [sql|
       SELECT work_id, revision_id,
         name.name, comment, work_type_id, language_id
       FROM work
       JOIN work_revision USING (work_id)
       JOIN work_tree USING (work_tree_id)
       JOIN work_data USING (work_data_id)
       JOIN work_name name ON (work_data.name = name.id)
       WHERE revision_id = ? |]

instance ViewTree Work where
  viewTree r = WorkTree <$> fmap coreData (viewRevision r)
                        <*> viewRelationships r
                        <*> viewAliases r
                        <*> viewAnnotation r
                        <*> viewIswcs r

instance Editable Work


--------------------------------------------------------------------------------
newtype WorkType = WorkType { workTypeName :: Text }
  deriving (Eq, Show)

instance Referenceable WorkType where
  type RefSpec WorkType = Int

instance FromField (Ref WorkType) where
  fromField f v = view reference <$> fromField f v

instance FromRow WorkType where
  fromRow = WorkType <$> field

instance ToField (Ref WorkType) where
  toField = toField . dereference

instance ToRow WorkType where
  toRow WorkType{..} = [ toField workTypeName
                       ]

instance Add WorkType where
  add workType = head <$>
    query [sql| INSERT INTO work_type (name) VALUES (?)
                RETURNING id, name |] workType

instance ResolveReference WorkType where
  resolveReference workTypeId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM work_type WHERE id = ? |] (Only workTypeId)


--------------------------------------------------------------------------------
viewIswcs :: (Functor m, Monad m, MonadIO m)
  => Ref (Revision Work) -> MusicBrainzT m (Set.Set ISWC)
viewIswcs revisionId = Set.fromList . map fromOnly <$> query q (Only revisionId)
  where
    q = [sql| SELECT iswc
              FROM iswc
              JOIN work_revision USING (work_tree_id)
              WHERE revision_id = ? |]


--------------------------------------------------------------------------------
findIswcs :: (Functor m, MonadIO m, Monad m)
  => Set.Set (Ref (Revision Work))
  -> MusicBrainzT m (Map.Map (Ref (Revision Work)) (Set.Set ISWC))
findIswcs revisionIds =
    associate <$> query q (Only $ In (Set.toList revisionIds))
  where
    associate =
      Map.fromList .
        map (fst . head &&& Set.fromList . map snd) .
          groupBy ((==) `on` fst)
    q = [sql| SELECT revision_id, iswc
              FROM iswc
              JOIN work_revision USING (work_tree_id)
              WHERE revision_id IN ? |]


--------------------------------------------------------------------------------
findByArtist :: (Functor m, MonadIO m) => Ref Artist -> MusicBrainzT m [CoreEntity Work]
findByArtist artistId = query q (artistId, artistId)
  where
    q = [sql|
          SELECT work_id, revision_id,
            name.name, comment, work_type_id, language_id
          FROM (
            SELECT DISTINCT work_id, work_revision.revision_id, work_tree_id
            FROM l_work_recording
            JOIN recording USING (recording_id)
            JOIN recording_revision USING (recording_id)
            JOIN recording_tree USING (recording_tree_id)
            JOIN recording_data USING (recording_data_id)
            JOIN artist_credit_name USING (artist_credit_id)
            JOIN work_revision USING (work_tree_id)
            JOIN work USING (work_id)
            WHERE artist_id = ?
              AND recording_revision.revision_id = recording.master_revision_id
              AND work_revision.revision_id = work.master_revision_id

            UNION

            SELECT DISTINCT work_id, work_revision.revision_id, work_tree_id
            FROM l_work_artist
            JOIN work_revision USING (work_tree_id)
            JOIN work USING (work_id)
            WHERE artist_id = ?
              AND work_revision.revision_id = work.master_revision_id
          ) q
          JOIN work_tree USING (work_tree_id)
          JOIN work_data USING (work_data_id)
          JOIN work_name name ON (work_data.name = name.id)
          ORDER BY
            musicbrainz_collate(name.name)
        |]


--------------------------------------------------------------------------------
findByIswc :: (Functor m, MonadIO m) => ISWC -> MusicBrainzT m [CoreEntity Work]
findByIswc iswc = query q (Only iswc)
  where
    q = [sql|
          SELECT work_id, revision_id,
            name.name, comment, work_type_id, language_id
          FROM (
            SELECT DISTINCT work_id, work_revision.revision_id, work_tree_id
            FROM work
            JOIN work_revision USING (work_id)
            JOIN iswc USING (work_tree_id)
            WHERE iswc.iswc = ?
              AND work_revision.revision_id = work.master_revision_id
          ) q
          JOIN work_tree USING (work_tree_id)
          JOIN work_data USING (work_data_id)
          JOIN work_name name ON (work_data.name = name.id)
          ORDER BY
            musicbrainz_collate(name.name)
        |]
