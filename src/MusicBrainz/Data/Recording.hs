{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Functions for interacting with 'Recording's in the MusicBrainz database. -}
module MusicBrainz.Data.Recording
    ( create ) where

import Control.Applicative
import Control.Monad
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz
import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.Revision (newRevision)

instance FindLatest Recording where
  findLatest mbid = listToMaybe <$> query q (Only mbid)
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
{-| Create an entirely new recording, returning the final 'CoreEntity' as it is
in the database. -}
create :: Ref Editor -> Recording -> MusicBrainz (CoreEntity Recording)
create editor recording = do
  recordingTreeId <- recordingTree recording
  recordingId <- reserveRecording
  revisionId <- newRevision editor >>= newRecordingRevision recordingId recordingTreeId
  linkRevision recordingId revisionId
  return CoreEntity { coreMbid = recordingId
                    , coreRevision = revisionId
                    , coreData = recording
                    }
  where
    reserveRecording :: MusicBrainz (MBID Recording)
    reserveRecording = selectValue $
      query_ [sql| INSERT INTO recording (master_revision_id) VALUES (-1) RETURNING recording_id |]

    newRecordingRevision :: MBID Recording -> Int -> Ref (Revision Recording) -> MusicBrainz (Ref (Revision Recording))
    newRecordingRevision recordingId recordingTreeId revisionId = selectValue $
      query [sql| INSERT INTO recording_revision (recording_id, revision_id, recording_tree_id)
                  VALUES (?, ?, ?) RETURNING revision_id |]
        (recordingId, revisionId, recordingTreeId)


    linkRevision :: MBID Recording -> Ref (Revision Recording) -> MusicBrainz ()
    linkRevision recordingId revisionId = void $
      execute [sql| UPDATE recording SET master_revision_id = ? WHERE recording_id = ? |] (revisionId, recordingId)


--------------------------------------------------------------------------------
recordingTree :: Recording -> MusicBrainz Int
recordingTree recording = findOrInsertRecordingData >>= findOrInsertRecordingTree
  where
    findOrInsertRecordingData :: MusicBrainz Int
    findOrInsertRecordingData = selectValue $
      query [sql| SELECT find_or_insert_recording_data(?, ?, ?, ?) |]
        recording

    findOrInsertRecordingTree dataId = selectValue $
      query [sql| SELECT find_or_insert_recording_tree(?) |]
        (Only dataId)
