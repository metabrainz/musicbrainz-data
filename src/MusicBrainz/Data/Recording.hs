{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Functions for interacting with 'Recording's in the MusicBrainz database.

The majority of operations on recordings are common for all core entities, so you
should see the documentation on the 'Recording' type and notice all the type class
instances. -}
module MusicBrainz.Data.Recording
    ( viewIsrcs ) where

import Control.Applicative
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Foldable (forM_)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import qualified Data.Set as Set

import MusicBrainz
import MusicBrainz.Data.Annotation
import MusicBrainz.Data.Create
import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.Revision.Internal
import MusicBrainz.Data.Update
import MusicBrainz.Data.Tree
import MusicBrainz.Edit

import qualified MusicBrainz.Data.Generic as Generic

--------------------------------------------------------------------------------
instance FindLatest Recording where
  findLatest recordingId = head <$> query q (Only recordingId)
    where q = [sql|
       SELECT recording_id, revision_id,
        name.name, comment, artist_credit_id, length
      FROM recording
      JOIN recording_revision USING (recording_id)
      JOIN recording_tree USING (recording_tree_id)
      JOIN recording_data USING (recording_data_id)
      JOIN track_name name ON (recording_data.name = name.id)
      WHERE recording_id = ?
        AND revision_id = master_revision_id  |]


--------------------------------------------------------------------------------
instance Create Recording where
  create = Generic.create "recording"


--------------------------------------------------------------------------------
instance NewEntityRevision Recording where
  newEntityRevision revisionId recordingId recordingTreeId = void $
    execute [sql| INSERT INTO recording_revision (recording_id, revision_id, recording_tree_id)
                  VALUES (?, ?, ?) |]
          (recordingId, revisionId, recordingTreeId)


--------------------------------------------------------------------------------
instance MasterRevision Recording where
  setMasterRevision = Generic.setMasterRevision "recording"


--------------------------------------------------------------------------------
instance RealiseTree Recording where
  realiseTree recording = do
    dataId <- insertRecordingData (recordingData recording)
    treeId <- insertRecordingTree (recordingAnnotation recording) dataId
    realiseIsrcs treeId
    return treeId
    where
      insertRecordingData :: (Functor m, MonadIO m) => Recording -> MusicBrainzT m Int
      insertRecordingData data' = selectValue $
        query [sql| SELECT find_or_insert_recording_data(?, ?, ?, ?) |]
          data'

      insertRecordingTree annotation dataId = selectValue $
        query [sql| INSERT INTO recording_tree (recording_data_id, annotation)
                    VALUES (?, ?)
                    RETURNING recording_tree_id  |]
          (dataId, annotation)

      realiseIsrcs treeId = forM_ (recordingIsrcs recording) $ \isrc ->
        execute q (treeId, isrc)
        where q = [sql| INSERT INTO isrc (recording_tree_id, isrc) VALUES (?, ?) |]


--------------------------------------------------------------------------------
instance ViewAnnotation Recording where
  viewAnnotation = Generic.viewAnnotation "recording"


--------------------------------------------------------------------------------
instance Update Recording where
  update editor baseRev recording = do
    treeId <- realiseTree recording
    revisionId <- newChildRevision editor baseRev treeId
    includeRevision revisionId
    return revisionId


--------------------------------------------------------------------------------
instance ViewRevision Recording where
  viewRevision revisionId = head <$> query q (Only revisionId)
    where q = [sql|
       SELECT recording_id, revision_id,
        name.name, comment, artist_credit_id, length
      FROM recording
      JOIN recording_revision USING (recording_id)
      JOIN recording_tree USING (recording_tree_id)
      JOIN recording_data USING (recording_data_id)
      JOIN track_name name ON (recording_data.name = name.id)
      WHERE revision_id = ? |]


--------------------------------------------------------------------------------
instance Editable Recording where
  linkRevisionToEdit = Generic.linkRevisionToEdit "edit_recording"


--------------------------------------------------------------------------------
instance ViewTree Recording where
  viewTree r = RecordingTree <$> fmap coreData (viewRevision r)
                             <*> viewAnnotation r
                             <*> viewIsrcs r


--------------------------------------------------------------------------------
viewIsrcs :: (Functor m, Monad m, MonadIO m)
  => Ref (Revision Recording) -> MusicBrainzT m (Set.Set ISRC)
viewIsrcs revisionId = Set.fromList . map fromOnly <$> query q (Only revisionId)
  where q = [sql| SELECT isrc
                  FROM isrc
                  JOIN recording_revision USING (recording_tree_id)
                  WHERE revision_id = ?
            |]
