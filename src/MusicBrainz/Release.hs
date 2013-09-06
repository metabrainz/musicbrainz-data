{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module MusicBrainz.Release where

import Control.Applicative
import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Char
import Data.Foldable (forM_)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid (mempty)
import Data.Set (Set)
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Database.PostgreSQL.Simple ((:.)(..), In(..), Only(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.ToRow (ToRow(..))

import MusicBrainz.Util (groupMap, groupRows, viewOnce)
import MusicBrainz.Merge
import MusicBrainz.Monad
import MusicBrainz.Annotation
import MusicBrainz.Artist
import MusicBrainz.ArtistCredit (ArtistCredit)
import MusicBrainz.Class.Create
import MusicBrainz.Class.FindLatest
import MusicBrainz.Class.MasterRevision
import MusicBrainz.Class.NewEntityRevision
import MusicBrainz.Class.RealiseTree
import MusicBrainz.Class.ResolveReference
import MusicBrainz.Class.RootTable
import MusicBrainz.Class.Update
import MusicBrainz.Class.ViewRevision
import MusicBrainz.Country (Country)
import MusicBrainz.Edit (Editable(..))
import MusicBrainz.Entity
import MusicBrainz.Label (Label)
import MusicBrainz.Language (Language)
import MusicBrainz.Lens (fieldFromPrism)
import MusicBrainz.MBID (MBID)
import MusicBrainz.PartialDate (PartialDate)
import MusicBrainz.Recording (Recording)
import MusicBrainz.Ref (Ref, Referenceable(..), reference, dereference)
import MusicBrainz.Relationship
import MusicBrainz.Relationship.Internal (HoldsRelationships(..), viewRelationships)
import MusicBrainz.ReleaseGroup (ReleaseGroup)
import MusicBrainz.Revision (Revision)
import MusicBrainz.Revision.Internal (CloneRevision(..))
import MusicBrainz.Script (Script)
import MusicBrainz.Tree

import {-# SOURCE #-} qualified MusicBrainz.Generic as Generic

import qualified Data.Map as Map
import qualified Data.Set as Set

--------------------------------------------------------------------------------
{-| A release in MusicBrainz is a physical product that people can purchase,
and belongs to a 'ReleaseGroup' and consists of some information along with
multiple 'Medium's. -}
data Release = Release
    { releaseName :: !Text
    , releaseComment :: !Text
    , releaseArtistCredit :: !(Ref ArtistCredit)
    , releaseReleaseGroup :: !(Ref ReleaseGroup)
    , releaseDate :: !PartialDate
    , releaseCountry :: !(Maybe (Ref Country))
    , releaseScript :: !(Maybe (Ref Script))
    , releaseLanguage :: !(Maybe (Ref Language))
    , releasePackaging :: !(Maybe (Ref ReleasePackaging))
    , releaseStatus :: !(Maybe (Ref ReleaseStatus))
    , releaseBarcode :: !(Maybe Barcode)
    }
  deriving (Eq, Show)

instance Referenceable Release where
  type RefSpec Release = MBID Release

instance FromField (Ref Release) where
  fromField f v = view reference <$> fromField f v

instance FromRow Release where
  fromRow = Release <$> field <*> field <*> field <*> field <*> fromRow
                    <*> field <*> field <*> field <*> field <*> field
                    <*> field

instance ToRow Release where
  toRow Release{..} = [ toField releaseName
                      , toField releaseComment
                      , toField releaseArtistCredit
                      ]
                      ++ toRow releaseDate
                      ++
                      [ toField releaseCountry
                      , toField releaseScript
                      , toField releaseLanguage
                      , toField releasePackaging
                      , toField releaseStatus
                      , toField releaseBarcode
                      ]

instance HasTree Release where
  data Tree Release =
    ReleaseTree { releaseData :: !Release
                , releaseRelationships :: !(Set LinkedRelationship)
                , releaseAnnotation :: !Text
                , releaseLabels :: !(Set ReleaseLabel)
                , releaseMediums :: ![Medium]
                }

  treeData ReleaseTree{..} = releaseData

instance TreeAnnotation Release where
  annotation f release =
    f (releaseAnnotation release) <&> \b -> release { releaseAnnotation = b }

instance TreeRelationships Release where
  relationships f release =
    f (releaseRelationships release) <&> \b -> release { releaseRelationships = b }

instance RootTable Release where
  rootTable = Tagged "release"

instance Mergeable (Tree Release) where
  type MergeRender (Tree Release) mo =
    ( Render (Maybe Barcode) mo
    , Render (Maybe (Ref Country)) mo
    , Render (Maybe (Ref Language)) mo
    , Render (Maybe (Ref ReleasePackaging)) mo
    , Render (Maybe (Ref ReleaseStatus)) mo
    , Render (Maybe (Ref Script)) mo
    , Render [Medium] mo
    , Render PartialDate mo
    , Render (Ref ArtistCredit) mo
    , Render (Ref ReleaseGroup) mo
    , Render (Set.Set LinkedRelationship) mo
    , Render (Set.Set ReleaseLabel) mo
    , Render Text mo
    )

  merge =
    ReleaseTree <$> releaseData `mergedVia` mergeReleaseData
                <*> releaseRelationships `mergedVia` merge
                <*> releaseAnnotation `mergedVia` mergeEq
                <*> releaseLabels `mergedVia` merge
                <*> releaseMediums `mergedVia` mergeEq
    where
      mergeReleaseData =
        Release
              <$> releaseName `mergedVia` mergeEq
              <*> releaseComment `mergedVia` mergeEq
              <*> releaseArtistCredit `mergedVia` mergeEq
              <*> releaseReleaseGroup `mergedVia` mergeEq
              <*> releaseDate `mergedVia` mergeEq
              <*> releaseCountry `mergedVia` mergeEq
              <*> releaseScript `mergedVia` mergeEq
              <*> releaseLanguage `mergedVia` mergeEq
              <*> releasePackaging `mergedVia` mergeEq
              <*> releaseStatus `mergedVia` mergeEq
              <*> releaseBarcode `mergedVia` mergeEq

instance HoldsRelationships Release where
  reflectRelationshipChange = Generic.reflectRelationshipChange ReleaseRelationship

instance FindLatest Release where
  findLatest = Generic.findLatest
    [sql|
      SELECT release_id, revision_id,
         name.name, comment, artist_credit_id, release_group_id,
         date_year, date_month, date_day, country_id, script_id, language_id,
         release_packaging_id, release_status_id, barcode
      FROM release
      JOIN release_revision USING (release_id)
      JOIN release_tree USING (release_tree_id)
      JOIN release_data USING (release_data_id)
      JOIN release_name name ON (release_data.name = name.id)
      WHERE release_id IN ?
        AND revision_id = master_revision_id  |]

instance NewEntityRevision Release where
  newEntityRevision revisionId releaseId releaseTreeId = void $
    execute [sql| INSERT INTO release_revision (release_id, revision_id, release_tree_id)
                  VALUES (?, ?, ?) |]
      (releaseId, revisionId, releaseTreeId)

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

      insertReleaseTree annotationBody releaseGroupId dataId = selectValue $
        query [sql| INSERT INTO release_tree (release_data_id, release_group_id, annotation)
                    VALUES (?, ?, ?)
                    RETURNING release_tree_id  |]
          (dataId, releaseGroupId, annotationBody)

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

instance Editable Release

instance ViewTree Release where
  viewTree r = ReleaseTree <$> fmap coreData (viewRevision r)
                           <*> viewRelationships r
                           <*> viewAnnotation r
                           <*> viewOnce viewReleaseLabels r
                           <*> viewOnce viewMediums r

instance ViewAnnotation Release

instance CloneRevision Release

instance Create Release

instance MasterRevision Release

instance ResolveReference (Revision Release)

instance ResolveReference Release

instance Update Release


--------------------------------------------------------------------------------
data Barcode = Barcode [Int] | NoBarcode
  deriving (Eq, Ord, Show, Typeable)

barcode :: Prism' String Barcode
barcode = prism showBarcode parseBarcode
  where
    showBarcode (Barcode bs) = map intToDigit bs
    showBarcode NoBarcode = []

    parseBarcode bs =
      maybe (Left bs) (Right . makeBarcode) $
        sequence $
          map (\b -> if isDigit b then Just (digitToInt b) else Nothing) bs

    makeBarcode [] = NoBarcode
    makeBarcode b = Barcode b

instance FromField Barcode where
  fromField = fieldFromPrism barcode

instance ToField Barcode where
  toField = toField . view (re barcode)

instance ToField (Ref Release) where
  toField = toField . dereference


--------------------------------------------------------------------------------
{-| The type of packaging a release came in. -}
data ReleasePackaging = ReleasePackaging { releasePackagingName :: !Text }
  deriving (Eq, Show)

instance Referenceable ReleasePackaging where
  type RefSpec ReleasePackaging = Int

instance FromField (Ref ReleasePackaging) where
  fromField f v = view reference <$> fromField f v

instance FromRow ReleasePackaging where
  fromRow = ReleasePackaging <$> field

instance ToField (Ref ReleasePackaging) where
  toField = toField . dereference

instance ToRow ReleasePackaging where
  toRow ReleasePackaging{..} = [ toField releasePackagingName
                               ]

instance Add ReleasePackaging where
  add rp = head <$>
    query [sql| INSERT INTO release_packaging (name) VALUES (?)
                RETURNING id, name |] rp

instance ResolveReference ReleasePackaging where
  resolveReference releasePackagingId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM release_packaging WHERE id = ? |] (Only releasePackagingId)

--------------------------------------------------------------------------------
{-| A release status indicates whether a 'Release' was released official,
promotionally, as a bootleg, etc. -}
data ReleaseStatus = ReleaseStatus { releaseStatusName :: !Text }
  deriving (Eq, Show)

instance Referenceable ReleaseStatus where
  type RefSpec ReleaseStatus = Int

instance FromField (Ref ReleaseStatus) where
  fromField f v = view reference <$> fromField f v

instance FromRow ReleaseStatus where
  fromRow = ReleaseStatus <$> field

instance ToField (Ref ReleaseStatus) where
  toField = toField . dereference

instance ToRow ReleaseStatus where
  toRow ReleaseStatus{..} = [ toField releaseStatusName
                            ]

instance Add ReleaseStatus where
  add rs = head <$>
    query [sql| INSERT INTO release_status (name) VALUES (?)
                RETURNING id, name |] rs

instance ResolveReference ReleaseStatus where
  resolveReference releaseStatusId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM release_status WHERE id = ? |] (Only releaseStatusId)

--------------------------------------------------------------------------------
data ReleaseLabel = ReleaseLabel
    { releaseLabel :: !(Maybe (Ref Label))
    , releaseCatalogNumber :: !(Maybe Text)
    }
  deriving (Eq, Ord, Show)

instance FromRow ReleaseLabel where
  fromRow = ReleaseLabel <$> field <*> field

instance ToRow ReleaseLabel where
  toRow ReleaseLabel{..} = [ toField releaseLabel
                           , toField releaseCatalogNumber
                           ]

--------------------------------------------------------------------------------
data Medium = Medium
    { mediumName :: !Text
    , mediumFormat :: !(Maybe (Ref MediumFormat))
    , mediumPosition :: !Int
    , mediumTracks :: ![Track]
    , mediumCdTocs :: !(Set CdToc)
    }
  deriving (Eq, Show)

instance ToRow Medium where
  toRow Medium{..} = [ toField mediumPosition
                     , toField mediumName
                     , toField mediumFormat
                     ]

instance Mergeable Medium where
  type MergeRender Medium mo =
    ( Render Int mo
    , Render (Maybe (Ref MediumFormat)) mo
    , Render (Set.Set CdToc) mo
    , Render [Track] mo
    , Render Text mo
    )
  merge = Medium <$> mediumName `mergedVia` mergeEq
                 <*> mediumFormat `mergedVia` mergeEq
                 <*> mediumPosition `mergedVia` mergeEq
                 <*> mediumTracks `mergedVia` merge
                 <*> mediumCdTocs `mergedVia` merge

--------------------------------------------------------------------------------
data MediumFormat = MediumFormat { mediumFormatName :: !Text }
  deriving (Eq, Show)

instance Referenceable MediumFormat where
  type RefSpec MediumFormat = Int

instance FromField (Ref MediumFormat) where
  fromField f v = view reference <$> fromField f v

instance FromRow MediumFormat where
  fromRow = MediumFormat <$> field

instance ToField (Ref MediumFormat) where
  toField = toField . dereference

instance ToRow MediumFormat where
  toRow MediumFormat{..} = [ toField mediumFormatName ]

instance Add MediumFormat where
  add mf = head <$>
    query [sql| INSERT INTO medium_format (name) VALUES (?) RETURNING id, name |]
      mf

instance ResolveReference MediumFormat where
  resolveReference mfId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM medium_format WHERE id = ? |] (Only mfId)

--------------------------------------------------------------------------------
data Track = Track
    { trackName :: !Text
    , trackRecording :: !(Ref Recording)
    , trackDuration :: !(Maybe Int)
    , trackArtistCredit :: !(Ref ArtistCredit)
    , trackPosition :: !Text
    }
  deriving (Eq, Show)

instance FromRow Track where
  fromRow = Track <$> field <*> field <*> field <*> field <*> field

instance ToRow Track where
  toRow Track{..} = [ toField trackName
                    , toField trackRecording
                    , toField trackDuration
                    , toField trackArtistCredit
                    , toField trackPosition
                    ]

instance Mergeable Track where
  type MergeRender Track mo =
    ( Render (Maybe Int) mo
    , Render (Ref ArtistCredit) mo
    , Render (Ref Recording) mo
    , Render Text mo
    )
  merge = Track <$> trackName `mergedVia` mergeEq
                <*> trackRecording `mergedVia` mergeEq
                <*> trackDuration `mergedVia` mergeEq
                <*> trackArtistCredit `mergedVia` mergeEq
                <*> trackPosition `mergedVia` mergeEq

--------------------------------------------------------------------------------
data CdToc = CdToc
    { cdTocTrackOffsets :: !(Vector Int)
    , cdTocLeadoutOffset :: !Int
    }
  deriving (Eq, Ord, Show)

instance FromRow CdToc where
  fromRow = CdToc <$> field <*> field

instance ToRow CdToc where
  toRow CdToc{..} = [ toField cdTocTrackOffsets
                    , toField cdTocLeadoutOffset
                    ]


--------------------------------------------------------------------------------
viewReleaseLabels :: (Functor m, Monad m, MonadIO m)
  => Set.Set (Ref (Revision Release))
  -> MusicBrainzT m (Map.Map (Ref (Revision Release)) (Set.Set ReleaseLabel))
viewReleaseLabels r =
    groupMap partition r' <$> query q r'
  where
    r' = Set.toList r

    q = [sql| SELECT revision_id, label_id, catalog_number
              FROM release_label
              JOIN release_revision USING (release_tree_id)
              WHERE revision_id = ? |]

    partition ((Only revisionId) :. rl) = (revisionId, Set.singleton rl)


--------------------------------------------------------------------------------
viewMediums :: (Applicative m, Functor m, Monad m, MonadIO m)
  => Set.Set (Ref (Revision Release))
  -> MusicBrainzT m (Map.Map (Ref (Revision Release)) [Medium])
viewMediums revisionIds = do
  mediums <- query selectMediums (Only $ In revisionIds')
  tracks <- groupTracks <$>
    query selectTracks (Only $ In (mediums ^.. traverse._2 :: [Int]))
  cdtocs <- groupCdTocs <$> query selectCdTocs (Only $ In revisionIds')
  pure $ associateMediums mediums tracks cdtocs revisionIds'

  where
    revisionIds' = Set.toList revisionIds

    selectMediums =
      [sql| SELECT release_tree_id, tracklist_id, name, medium_format_id, position, revision_id
            FROM medium
            JOIN release_revision
            USING (release_tree_id)
            WHERE revision_id = ?
            ORDER by release_tree_id, position ASC
          |]

    selectTracks =
      [sql| SELECT tracklist_id, track_name.name, recording_id, length, artist_credit_id, number
            FROM track
            JOIN track_name ON (track_name.id = track.name)
            WHERE tracklist_id IN ?
            ORDER BY tracklist_id, position ASC
          |]

    selectCdTocs =
      [sql| SELECT release_tree_id, position, track_offsets, leadout_offset
            FROM medium_cdtoc
            JOIN release_revision USING (release_tree_id)
            WHERE revision_id = ?
          |]

    groupTracks = groupRows $
      \(Only tracklistId :. track) -> (tracklistId :: Int, [track])

    groupCdTocs = groupRows $
      \((releaseTreeId, position) :. cdtoc) ->
        ( (releaseTreeId :: Ref (Tree Release), position :: Int)
        , Set.singleton cdtoc
        )

    associateMediums mediums tracks cdtocs ks =
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
      in groupMap
           (\(releaseTreeId, tracklistId, name, format, position, revision) ->
               (revision, [formMedium (releaseTreeId, tracklistId, name, format, position)]))
           ks
           mediums


--------------------------------------------------------------------------------
findByReleaseGroup :: (Functor m, MonadIO m) => Ref ReleaseGroup -> MusicBrainzT m [CoreEntity Release]
findByReleaseGroup rgId = query q (Only rgId)
  where
    q = [sql|
          SELECT release_id, revision_id,
            name.name, comment, artist_credit_id, release_group_id,
            date_year, date_month, date_day, country_id, script_id, language_id,
            release_packaging_id, release_status_id, barcode
          FROM release
          JOIN release_revision USING (release_id)
          JOIN release_tree USING (release_tree_id)
          JOIN release_data USING (release_data_id)
          JOIN release_name name ON (release_data.name = name.id)
          LEFT JOIN country ON (country.id = release_data.country_id)
          WHERE release_group_id = ?
            AND master_revision_id = revision_id
          ORDER BY
            date_year, date_month, date_day,
            musicbrainz_collate(country.name),
            barcode
          |]


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


--------------------------------------------------------------------------------
findByArtist :: (Functor m, MonadIO m) => Ref Artist -> MusicBrainzT m [CoreEntity Release]
findByArtist artistId = query q (Only artistId)
  where
    q = [sql|
          SELECT release_id, revision_id,
            name.name, comment, artist_credit_id, release_group_id,
            date_year, date_month, date_day, country_id, script_id, language_id,
            release_packaging_id, release_status_id, barcode
          FROM (
            SELECT DISTINCT
              release_id, revision_id, release_tree_id,
              release_data.name, comment, artist_credit_id, release_group_id,
              date_year, date_month, date_day, country_id, script_id, language_id,
              release_packaging_id, release_status_id, barcode
            FROM release
            JOIN release_revision USING (release_id)
            JOIN release_tree USING (release_tree_id)
            JOIN release_data USING (release_data_id)
            JOIN artist_credit_name USING (artist_credit_id)
            WHERE artist_id = ? AND master_revision_id = revision_id
          ) q
          JOIN release_name name ON (q.name = name.id)
          LEFT JOIN country ON (country.id = country_id)
          ORDER BY
            date_year, date_month, date_day,
            musicbrainz_collate(country.name),
            barcode,
            musicbrainz_collate(name.name)
          |]
