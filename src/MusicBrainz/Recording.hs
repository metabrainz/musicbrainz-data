{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module MusicBrainz.Recording where

import Control.Applicative
import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (forM_)
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import Database.PostgreSQL.Simple (Only(..), (:.)(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.ToRow (ToRow(..))

import qualified Data.Set as Set

import MusicBrainz.Merge
import MusicBrainz.Monad
import MusicBrainz.Artist
import MusicBrainz.ArtistCredit
import MusicBrainz.Annotation
import MusicBrainz.Class.RootTable
import MusicBrainz.Class.Update
import MusicBrainz.ISRC
import MusicBrainz.MBID (MBID)
import MusicBrainz.Relationship
import MusicBrainz.Relationship.Internal
import MusicBrainz.Versioning hiding (merge)

import {-# SOURCE #-} MusicBrainz.Release
import {-# SOURCE #-} qualified MusicBrainz.Generic as Generic

--------------------------------------------------------------------------------
{-| A recording in MusicBrainz (which is realised on 'Tracklist' as a
'Track'. -}
data Recording = Recording
    { recordingName :: !Text
    , recordingComment :: !Text
    , recordingArtistCredit :: !(Ref ArtistCredit)
    , recordingDuration :: !(Maybe Int)
    }
  deriving (Eq, Show)

instance Referenceable Recording where
  type RefSpec Recording = MBID Recording

instance FromField (Ref Recording) where
  fromField f v = view reference <$> fromField f v

instance FromRow Recording where
  fromRow = Recording <$> field <*> field <*> field <*> field

instance ToField (Ref Recording) where
  toField = toField . dereference

instance ToRow Recording where
  toRow Recording{..} = [ toField recordingName
                        , toField recordingComment
                        , toField recordingArtistCredit
                        , toField recordingDuration
                        ]

instance HasTree Recording where
  data Tree Recording =
    RecordingTree { recordingData :: !Recording
                  , recordingRelationships :: !(Set.Set LinkedRelationship)
                  , recordingAnnotation :: !Text
                  , recordingIsrcs :: !(Set.Set ISRC)
                  }

  treeData RecordingTree{..} = recordingData

instance TreeAnnotation Recording where
  annotation f recording =
    f (recordingAnnotation recording) <&> \b -> recording { recordingAnnotation = b }

instance TreeRelationships Recording where
  relationships f recording =
    f (recordingRelationships recording) <&> \b -> recording { recordingRelationships = b }

instance Mergeable (Tree Recording) where
  type MergeRender (Tree Recording) mo =
    ( Render (Maybe Int) mo
    , Render (Ref ArtistCredit) mo
    , Render (Set.Set ISRC) mo
    , Render (Set.Set LinkedRelationship) mo
    , Render Text mo
    )

  merge =
    RecordingTree <$> recordingData `mergedVia` mergeRecordingData
                  <*> recordingRelationships `mergedVia` merge
                  <*> recordingAnnotation `mergedVia` mergeEq
                  <*> recordingIsrcs `mergedVia` merge
    where
      mergeRecordingData =
        Recording
              <$> recordingName `mergedVia` mergeEq
              <*> recordingComment `mergedVia` mergeEq
              <*> recordingArtistCredit `mergedVia` mergeEq
              <*> recordingDuration `mergedVia` mergeEq

instance CloneRevision Recording

instance Create Recording

instance MasterRevision Recording

instance ResolveReference (Revision Recording)

instance ResolveReference Recording

instance Update Recording

instance ViewAnnotation Recording

instance RootTable Recording where
  rootTable = Tagged "recording"

instance HoldsRelationships Recording where
  reflectRelationshipChange = Generic.reflectRelationshipChange RecordingRelationship

instance FindLatest Recording where
  findLatest = Generic.findLatest
    [sql|
      SELECT recording_id, revision_id,
        name.name, comment, artist_credit_id, length
      FROM recording
      JOIN recording_revision USING (recording_id)
      JOIN recording_tree USING (recording_tree_id)
      JOIN recording_data USING (recording_data_id)
      JOIN track_name name ON (recording_data.name = name.id)
      WHERE recording_id IN ?
        AND revision_id = master_revision_id  |]

instance NewEntityRevision Recording where
  newEntityRevision revisionId recordingId recordingTreeId = void $
    execute [sql| INSERT INTO recording_revision (recording_id, revision_id, recording_tree_id)
                  VALUES (?, ?, ?) |]
          (recordingId, revisionId, recordingTreeId)

instance RealiseTree Recording where
  realiseTree recording = do
    dataId <- insertRecordingData (recordingData recording)
    treeId <- insertRecordingTree (recordingAnnotation recording) dataId
    Generic.realiseRelationships "recording" treeId recording
    realiseIsrcs treeId
    return treeId
    where
      insertRecordingData data' = fmap (`asTypeOf` (1 :: Int)) <$> selectValue $
        query [sql| SELECT find_or_insert_recording_data(?, ?, ?, ?) |]
          data'

      insertRecordingTree annotationBody dataId = selectValue $
        query [sql| INSERT INTO recording_tree (recording_data_id, annotation)
                    VALUES (?, ?)
                    RETURNING recording_tree_id  |]
          (dataId, annotationBody)

      realiseIsrcs treeId = forM_ (recordingIsrcs recording) $ \isrcCode ->
        execute q (treeId, isrcCode)
        where q = [sql| INSERT INTO isrc (recording_tree_id, isrc) VALUES (?, ?) |]

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

instance Editable Recording

instance ViewTree Recording where
  viewTree r = RecordingTree <$> fmap coreData (viewRevision r)
                             <*> viewRelationships r
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


--------------------------------------------------------------------------------
data RecordingUse = RecordingUse
    { recordingTrack :: Track
    , recordingTrackRelease :: CoreEntity Release
    , recordingMediumTrackCount :: Int
    }
  deriving (Eq, Show)

findRecordingTracks :: (MonadIO m, Functor m)
  => Ref Recording -> MusicBrainzT m [RecordingUse]
findRecordingTracks recordingId =
    map toUsage <$> query q (Only recordingId)
  where
    toUsage (track :. release :. (Only totalTracks)) =
      RecordingUse track release totalTracks
    q = [sql|
          SELECT
            track_name.name, track.recording_id, track.length, track.artist_credit_id,
            track.number,

            release.release_id, release_revision.revision_id,
            name.name, release_data.comment, release_data.artist_credit_id,
            release_tree.release_group_id, release_data.date_year,
            release_data.date_month, release_data.date_day, release_data.country_id,
            release_data.script_id, release_data.language_id,
            release_data.release_packaging_id, release_data.release_status_id,
            release_data.barcode,

            (SELECT count(*)
             FROM track
             JOIN medium USING (tracklist_id)
             WHERE medium.release_tree_id = release_tree.release_tree_id) total_tracks
          FROM track
          JOIN track_name ON (track_name.id = track.name)
          JOIN medium USING (tracklist_id)
          JOIN release_revision USING (release_tree_id)
          JOIN release_tree USING (release_tree_id)
          JOIN release_data USING (release_data_id)
          JOIN release USING (release_id)
          JOIN release_name name ON (name.id = release_data.name)
          WHERE recording_id = ?
            AND revision_id = master_revision_id
          ORDER BY date_year, date_month, date_day, musicbrainz_collate(name.name)
        |]


--------------------------------------------------------------------------------
findByArtist :: (Functor m, MonadIO m) => Ref Artist -> MusicBrainzT m [CoreEntity Recording]
findByArtist artistId = query q (Only artistId)
  where
    q = [sql|
          SELECT recording_id, revision_id,
            name.name, comment, artist_credit_id, length
          FROM (
            SELECT DISTINCT recording_id, revision_id,
              recording_data.name, comment, artist_credit_id, length
            FROM recording
            JOIN recording_revision USING (recording_id)
            JOIN recording_tree USING (recording_tree_id)
            JOIN recording_data USING (recording_data_id)
            JOIN artist_credit_name USING (artist_credit_id)
            WHERE artist_credit_name.artist_id = ?
              AND revision_id = master_revision_id
          ) q
          JOIN track_name name ON (q.name = name.id)
          ORDER BY
            musicbrainz_collate(name.name),
            musicbrainz_collate(comment)
        |]


--------------------------------------------------------------------------------
findByIsrc :: (Functor m, MonadIO m) => ISRC -> MusicBrainzT m [CoreEntity Recording]
findByIsrc isrcCode = query q (Only isrcCode)
  where
    q = [sql|
          SELECT recording_id, revision_id,
            name.name, comment, artist_credit_id, length
          FROM (
            SELECT DISTINCT recording_id, revision_id, recording_tree_id
            FROM recording
            JOIN recording_revision USING (recording_id)
            JOIN isrc USING (recording_tree_id)
            WHERE isrc.isrc = ?
              AND revision_id = master_revision_id
          ) q
          JOIN recording_tree USING (recording_tree_id)
          JOIN recording_data USING (recording_data_id)
          JOIN track_name name ON (recording_data.name = name.id)
          ORDER BY
            musicbrainz_collate(name.name),
            musicbrainz_collate(comment)
        |]
