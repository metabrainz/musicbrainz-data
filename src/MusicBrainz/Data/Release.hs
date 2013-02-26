{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Functions for interacting with 'Release's in the MusicBrainz database.

The majority of operations on releases are common for all core entities, so you
should see the documentation on the 'Release' type and notice all the type class
instances. -}
module MusicBrainz.Data.Release
    ( findByLabel
    , viewMediums
    , viewReleaseLabels
    ) where

import Control.Arrow ((&&&))
import Control.Applicative
import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Function
import Data.Foldable (foldMap, forM_)
import Data.List
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Database.PostgreSQL.Simple (Only(..), (:.)(..), In(..))
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
import MusicBrainz.Data.Update
import MusicBrainz.Data.Tree
import MusicBrainz.Edit

import qualified MusicBrainz.Data.Generic as Generic

--------------------------------------------------------------------------------
instance HoldsRelationships Release where
  fetchEndPoints = Generic.fetchEndPoints "release"
  reflectRelationshipChange = Generic.reflectRelationshipChange ReleaseRelationship


--------------------------------------------------------------------------------
instance FindLatest Release where
  findLatest releaseId = head <$> query q (Only releaseId)
    where q = [sql|
       SELECT release_id, revision_id,
         name.name, comment, artist_credit_id, release_group_id,
         date_year, date_month, date_day, country_id, script_id, language_id,
         release_packaging_id, release_status_id, barcode
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
    Generic.realiseRelationships "release" treeId release
    return treeId
    where
      insertReleaseData :: (Functor m, MonadIO m) => Release -> MusicBrainzT m Int
      insertReleaseData data' = selectValue $
        query [sql| SELECT find_or_insert_release_data(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) |]
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
        forM_ (zip (mediumTracks medium) [1..]) $ \(track, n) ->
          execute [sql| INSERT INTO track (tracklist_id, name, recording_id, length, artist_credit_id, position, number) VALUES (?, (SELECT find_or_insert_track_name(?)), ?, ?, ?, ?, ?) |]
            (Only tracklistId :. track :. Only (n :: Int))
        forM_ (mediumCdTocs medium) $ \cdtoc -> do
          execute [sql| INSERT INTO medium_cdtoc (release_tree_id, position, track_offsets, leadout_offset)
                        VALUES (?, ?, ?, ?) |]
            ((treeId, mediumPosition medium) :. cdtoc)


--------------------------------------------------------------------------------
instance ViewAnnotation Release where
  viewAnnotation = Generic.viewAnnotation "release"


--------------------------------------------------------------------------------
instance Update Release


--------------------------------------------------------------------------------
instance ViewRevision Release where
  viewRevision revisionId = head <$> query q (Only revisionId)
    where q = [sql|
       SELECT release_id, revision_id,
         name.name, comment, artist_credit_id, release_group_id,
         date_year, date_month, date_day, country_id, script_id, language_id,
         release_packaging_id, release_status_id, barcode
      FROM release
      JOIN release_revision USING (release_id)
      JOIN release_tree USING (release_tree_id)
      JOIN release_data USING (release_data_id)
      JOIN release_name name ON (release_data.name = name.id)
      WHERE revision_id = ? |]


--------------------------------------------------------------------------------
instance Editable Release where
  linkRevisionToEdit = Generic.linkRevisionToEdit "edit_release"

  change = prism ReleaseChange extract
    where extract a = case a of ReleaseChange c -> Right c
                                _ -> Left a


--------------------------------------------------------------------------------
instance ViewTree Release where
  viewTree r = ReleaseTree <$> fmap coreData (viewRevision r)
                           <*> viewRelationships r
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
    query selectTracks (Only $ In (mediums ^.. traverse._2 :: [Int]))
  cdtocs <- groupCdTocs <$> query selectCdTocs (Only revisionId)
  pure $ associateMediums mediums tracks cdtocs

  where
    selectMediums =
      [sql| SELECT release_tree_id, tracklist_id, name, medium_format_id, position
            FROM medium
            JOIN release_revision
            USING (release_tree_id)
            WHERE revision_id = ? |]

    selectTracks =
      [sql| SELECT tracklist_id, track_name.name, recording_id, length, artist_credit_id, number
            FROM track
            JOIN track_name ON (track_name.id = track.name)
            WHERE tracklist_id IN ? |]

    selectCdTocs =
      [sql| SELECT release_tree_id, position, track_offsets, leadout_offset
            FROM medium_cdtoc
            JOIN release_revision USING (release_tree_id)
            WHERE revision_id = ? |]

    groupRows splitRow =
      map (fst . head &&& foldMap snd) . groupBy ((==) `on` fst) . map splitRow

    groupTracks = groupRows $
      \(Only id' :. track) -> (id' :: Int, [track])

    groupCdTocs = groupRows $
      \((releaseTreeId, position) :. cdtoc) ->
        ( (releaseTreeId :: Ref (Tree Release), position :: Int)
        , Set.singleton cdtoc
        )

    associateMediums mediums tracks cdtocs =
      let formMedium (releaseTreeId, tracklistId, name, format, position) =
            Medium { mediumName = name
                   , mediumFormat = format
                   , mediumPosition = position
                   , mediumTracks =
                       fromMaybe (error "Tracklist association failed!") $
                         tracklistId `lookup` tracks
                   , mediumCdTocs =
                       fromMaybe mempty $
                         (releaseTreeId, position) `lookup` cdtocs

                   }
      in map formMedium mediums


--------------------------------------------------------------------------------
instance ResolveReference Release where
  resolveReference = Generic.resolveMbid "release"


--------------------------------------------------------------------------------
instance ResolveReference (Revision Release) where
  resolveReference = Generic.resolveRevision "release"


--------------------------------------------------------------------------------
instance CloneRevision Release where
  cloneRevision = Generic.cloneRevision "release"


--------------------------------------------------------------------------------
instance Merge Release



--------------------------------------------------------------------------------
findByLabel :: (Functor m, MonadIO m) => Ref Label -> MusicBrainzT m [CoreEntity Release]
findByLabel labelId = query q (Only labelId)
  where
    q = [sql|
          SELECT release_id, revision_id,
            name.name, comment, artist_credit_id, release_group_id,
            date_year, date_month, date_day, country_id, script_id, language_id,
            release_packaging_id, release_status_id, barcode
          FROM (
            SELECT DISTINCT ON (release_id)
              release_id, revision_id, release_tree_id, catalog_number
            FROM release
            JOIN release_revision USING (release_id)
            JOIN release_label USING (release_tree_id)
            WHERE label_id = ? AND master_revision_id = revision_id
          ) q
          JOIN release_tree USING (release_tree_id)
          JOIN release_data USING (release_data_id)
          JOIN release_name name ON (release_data.name = name.id)
          LEFT JOIN country ON (country.id = release_data.country_id)
          ORDER BY
            date_year, date_month, date_day, catalog_number,
            musicbrainz_collate(name.name),
            musicbrainz_collate(country.name),
            barcode
          |]
