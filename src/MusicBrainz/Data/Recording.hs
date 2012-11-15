{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Functions for interacting with 'Recording's in the MusicBrainz database. -}
module MusicBrainz.Data.Recording
    ( ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz
import MusicBrainz.Data.Create
import MusicBrainz.Data.FindLatest

import qualified MusicBrainz.Data.Generic.Create as GenericCreate

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
  create = GenericCreate.create GenericCreate.Specification
      { GenericCreate.getTree = recordingTree
      , GenericCreate.reserveEntity = GenericCreate.reserveEntityTable "recording"
      , GenericCreate.newEntityRevision = newRecordingRevision
      , GenericCreate.linkRevision = linkRevision
      }
    where
      newRecordingRevision recordingId recordingTreeId revisionId = selectValue $
        query [sql| INSERT INTO recording_revision (recording_id, revision_id, recording_tree_id)
                    VALUES (?, ?, ?) RETURNING revision_id |]
          (recordingId, revisionId, recordingTreeId)

      linkRevision recordingId revisionId = void $
        execute [sql| UPDATE recording SET master_revision_id = ? WHERE recording_id = ? |] (revisionId, recordingId)


--------------------------------------------------------------------------------
recordingTree :: (Functor m, MonadIO m)
  => Tree Recording -> MusicBrainzT m (Ref (Tree Recording))
recordingTree recording = findOrInsertRecordingData >>= findOrInsertRecordingTree
  where
    findOrInsertRecordingData :: (Functor m, MonadIO m) => MusicBrainzT m Int
    findOrInsertRecordingData = selectValue $
      query [sql| SELECT find_or_insert_recording_data(?, ?, ?, ?) |]
        (treeData recording)

    findOrInsertRecordingTree dataId = selectValue $
      query [sql| SELECT find_or_insert_recording_tree(?) |]
        (Only dataId)
