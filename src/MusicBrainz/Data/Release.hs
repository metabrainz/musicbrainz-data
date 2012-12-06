{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Functions for interacting with 'Release's in the MusicBrainz database.

The majority of operations on releases are common for all core entities, so you
should see the documentation on the 'Release' type and notice all the type class
instances. -}
module MusicBrainz.Data.Release
    ( viewMediums
    , viewReleaseLabels
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import Data.List
import Data.Maybe (fromMaybe)
import Data.Semigroup (First(..), sconcat)
import Database.PostgreSQL.Simple (Only(..), (:.)(..), In(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import qualified Data.List.NonEmpty as NonEmpty
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
instance FindLatest Release where
  findLatest releaseId = head <$> query q (Only releaseId)
    where q = [sql|
       SELECT release_id, revision_id,
         name.name, comment, artist_credit_id, release_group_id,
         date_year, date_month, date_day, country_id, script_id, language_id,
         release_packaging_id, release_status_id
      FROM release
      JOIN release_revision USING (release_id)
      JOIN release_tree USING (release_tree_id)
      JOIN release_data USING (release_data_id)
      JOIN release_name name ON (release_data.name = name.id)
      WHERE release_id = ?
        AND revision_id = master_revision_id  |]


--------------------------------------------------------------------------------
instance Create Release where
  create = Generic.create "release"


--------------------------------------------------------------------------------
instance NewEntityRevision Release where
  newEntityRevision revisionId releaseId releaseTreeId = void $
    execute [sql| INSERT INTO release_revision (release_id, revision_id, release_tree_id)
                  VALUES (?, ?, ?) |]
      (releaseId, revisionId, releaseTreeId)


--------------------------------------------------------------------------------
instance MasterRevision Release where
  setMasterRevision = Generic.setMasterRevision "release"


--------------------------------------------------------------------------------
instance RealiseTree Release where
  realiseTree release = do
    dataId <- insertReleaseData (releaseData release)
    treeId <- insertReleaseTree (releaseAnnotation release) (releaseReleaseGroup . releaseData $ release) dataId
    realiseReleaseLabels treeId
    realiseMediums treeId
    return treeId
    where
      insertReleaseData :: (Functor m, MonadIO m) => Release -> MusicBrainzT m Int
      insertReleaseData data' = selectValue $
        query [sql| SELECT find_or_insert_release_data(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) |]
          data'

      insertReleaseTree annotation releaseGroupId dataId = selectValue $
        query [sql| INSERT INTO release_tree (release_data_id, release_group_id, annotation)
                    VALUES (?, ?, ?)
                    RETURNING release_tree_id  |]
          (dataId, releaseGroupId, annotation)

      realiseReleaseLabels treeId = executeMany q params
        where
          params = map (Only treeId :.) (Set.toList $ releaseLabels release)
          q = [sql| INSERT INTO release_label (release_tree_id, label_id, catalog_number)
                    VALUES (?, ?, ?) |]

      realiseMediums treeId = forM_ (releaseMediums release) $ \medium -> do
        tracklistId <- selectValue $ query_ "INSERT INTO tracklist DEFAULT VALUES RETURNING id"
        execute [sql| INSERT INTO medium (position, name, medium_format_id, tracklist_id, release_tree_id) VALUES (?, ?, ?, ?, ?) |]
          (medium :. (tracklistId :: Int, treeId))
        forM_ (zip (mediumTracks medium) [1..]) $ \(track, n) -> do
          execute [sql| INSERT INTO track (tracklist_id, name, recording_id, length, artist_credit_id, position, number) VALUES (?, (SELECT find_or_insert_track_name(?)), ?, ?, ?, ?, ?) |] $
            (Only tracklistId :. track :. Only (n :: Int))


--------------------------------------------------------------------------------
instance ViewAnnotation Release where
  viewAnnotation = Generic.viewAnnotation "release"


--------------------------------------------------------------------------------
instance Update Release where
  update editor baseRev release = do
    treeId <- realiseTree release
    revisionId <- newChildRevision editor baseRev treeId
    includeRevision revisionId
    return revisionId


--------------------------------------------------------------------------------
instance ViewRevision Release where
  viewRevision revisionId = head <$> query q (Only revisionId)
    where q = [sql|
       SELECT release_id, revision_id,
         name.name, comment, artist_credit_id, release_group_id,
         date_year, date_month, date_day, country_id, script_id, language_id,
         release_packaging_id, release_status_id
      FROM release
      JOIN release_revision USING (release_id)
      JOIN release_tree USING (release_tree_id)
      JOIN release_data USING (release_data_id)
      JOIN release_name name ON (release_data.name = name.id)
      WHERE revision_id = ? |]


--------------------------------------------------------------------------------
instance Editable Release where
  linkRevisionToEdit = Generic.linkRevisionToEdit "edit_release"


--------------------------------------------------------------------------------
instance ViewTree Release where
  viewTree r = ReleaseTree <$> fmap coreData (viewRevision r)
                           <*> viewAnnotation r
                           <*> viewReleaseLabels r
                           <*> viewMediums r


--------------------------------------------------------------------------------
viewReleaseLabels :: (Functor m, Monad m, MonadIO m)
  => Ref (Revision Release) -> MusicBrainzT m (Set.Set ReleaseLabel)
viewReleaseLabels r = Set.fromList <$> query q (Only r)
  where q = [sql| SELECT label_id, catalog_number
                  FROM release_label
                  JOIN release_revision USING (release_tree_id)
                  WHERE revision_id = ? |]


--------------------------------------------------------------------------------
viewMediums :: (Applicative m, Functor m, Monad m, MonadIO m)
  => Ref (Revision Release) -> MusicBrainzT m [Medium]
viewMediums revisionId = do
  mediums <- query selectMediums (Only revisionId)
  tracks <- groupTracks <$>
    query selectTracks (Only $ In (mediums ^.. traverse._1 :: [Int]))
  pure $ associateMediums mediums tracks

  where
    selectMediums =
      [sql| SELECT tracklist_id, name, medium_format_id, position
            FROM medium
            JOIN release_revision
            USING (release_tree_id)
            WHERE revision_id = ? |]

    selectTracks =
      [sql| SELECT tracklist_id, track_name.name, recording_id, length, artist_credit_id, number
            FROM track
            JOIN track_name ON (track_name.id = track.name)
            WHERE tracklist_id IN ? |]

    -- We group tracks by using a pair of semigroups. The first element uses the
    -- 'First' semigroup to collapse a list of tracklist IDs into a single
    -- tracklist ID -- we've guaranteed these are equal due to the groupBy.
    -- The second semigroup is the list semigroup, so we simply concatenate all
    -- tracks with the same tracklist ID together.
    groupTracks =
      let formTrack (tracklistId, name, recording, duration, ac, position) =
            ( First (tracklistId :: Int)
            , [Track name recording duration ac position]
            )
      in map (over _1 getFirst . sconcat . NonEmpty.fromList) .
           groupBy ((==) `on` fst) . map formTrack

    associateMediums mediums tracks =
      let formMedium (tracklistId, name, format, position) =
            Medium { mediumName = name
                   , mediumFormat = format
                   , mediumPosition = position
                   , mediumTracks =
                       fromMaybe (error "Tracklist association failed!") $
                         tracklistId `lookup` tracks
                   }
      in map formMedium mediums
