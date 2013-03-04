{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions for interacting with MusicBrainz works in the database.

The majority of operations on works are common for all core entities, so you
should see the documentation on the 'Work' type and notice all the type class
instances. -}
module MusicBrainz.Data.Work
    ( findByArtist

      -- * ISWCs
    , findIswcs
    , viewIswcs
    ) where

import Control.Arrow ((&&&))
import Control.Applicative
import Control.Lens (prism)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (forM_)
import Data.Function (on)
import Data.List (groupBy)
import Database.PostgreSQL.Simple (In(..), Only(..))
import Database.PostgreSQL.Simple.SqlQQ

import qualified Data.Map as Map
import qualified Data.Set as Set

import MusicBrainz hiding (iswc)
import MusicBrainz.Data.Alias
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
instance HoldsRelationships Work where
  fetchEndPoints = Generic.fetchEndPoints "work"
  reflectRelationshipChange = Generic.reflectRelationshipChange WorkRelationship


--------------------------------------------------------------------------------
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


--------------------------------------------------------------------------------
instance Create Work where
  create = Generic.create "work"


--------------------------------------------------------------------------------
instance NewEntityRevision Work where
  newEntityRevision = Generic.newEntityRevision "work"


--------------------------------------------------------------------------------
instance MasterRevision Work where
  setMasterRevision = Generic.setMasterRevision "work"


--------------------------------------------------------------------------------
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

      insertWorkTree annotation dataId = selectValue $
        query [sql| INSERT INTO work_tree (work_data_id, annotation)
                    VALUES (?, ?)
                    RETURNING work_tree_id  |]
          (dataId, annotation)

      realiseIswcs treeId = forM_ (workIswcs work) $ \iswc ->
        execute q (treeId, iswc)
        where q = [sql| INSERT INTO iswc (work_tree_id, iswc) VALUES (?, ?) |]



--------------------------------------------------------------------------------
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


--------------------------------------------------------------------------------
instance ViewTree Work where
  viewTree r = WorkTree <$> fmap coreData (viewRevision r)
                        <*> viewRelationships r
                        <*> viewAliases r
                        <*> viewAnnotation r
                        <*> viewIswcs r


--------------------------------------------------------------------------------
instance Editable Work where
  linkRevisionToEdit = Generic.linkRevisionToEdit "edit_work"

  change = prism WorkChange extract
    where extract a = case a of WorkChange c -> Right c
                                _ -> Left a


--------------------------------------------------------------------------------
instance ViewAliases Work where
  viewAliases = Generic.viewAliases "work"


--------------------------------------------------------------------------------
instance ViewAnnotation Work where
  viewAnnotation = Generic.viewAnnotation "work"


--------------------------------------------------------------------------------
instance CloneRevision Work where
  cloneRevision = Generic.cloneRevision "work"


--------------------------------------------------------------------------------
instance Update Work


--------------------------------------------------------------------------------
instance ResolveReference Work where
  resolveReference = Generic.resolveMbid "work"


--------------------------------------------------------------------------------
instance ResolveReference (Revision Work) where
  resolveReference = Generic.resolveRevision "work"


--------------------------------------------------------------------------------
instance Merge Work


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
