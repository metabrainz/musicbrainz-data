{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module MusicBrainz.Artist
    ( Artist(..)
    , ArtistType(..)
    , Tree(ArtistTree)
    , artistData
    , artistRelationships
    , artistAliases
    , artistIpiCodes
    , artistIsniCodes
    , artistAnnotation
    ) where

import Control.Applicative
import Control.Lens hiding ((.>))
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.ToRow (ToRow(..))

import qualified Data.Set as Set

import MusicBrainz.Alias
import MusicBrainz.Annotation
import MusicBrainz.Class.RootTable
import MusicBrainz.Class.Update
import MusicBrainz.Country (Country)
import MusicBrainz.Gender (Gender)
import MusicBrainz.IPI
import MusicBrainz.ISNI
import MusicBrainz.MBID
import MusicBrainz.Merge
import MusicBrainz.Monad
import MusicBrainz.PartialDate (PartialDate)
import MusicBrainz.Relationship
import MusicBrainz.Relationship.Internal
import MusicBrainz.Util (viewOnce)
import MusicBrainz.Versioning hiding (merge)

import {-# SOURCE #-} qualified MusicBrainz.Generic as Generic

--------------------------------------------------------------------------------
{-| The data about an artist in MusicBrainz. -}
data Artist = Artist
    { artistName :: !Text
    , artistSortName :: !Text
    , artistComment :: !Text
    , artistBeginDate :: !PartialDate
    , artistEndDate :: !PartialDate
    , artistEnded :: !Bool
    , artistGender :: !(Maybe (Ref Gender))
    , artistType :: !(Maybe (Ref ArtistType))
    , artistCountry :: !(Maybe (Ref Country))
    }
  deriving (Eq, Show)

instance HasTree Artist where
  data Tree Artist =
    ArtistTree { artistData :: !Artist
               , artistRelationships :: !(Set LinkedRelationship)
               , artistAliases :: !(Set (Alias Artist))
               , artistIpiCodes :: !(Set IPI)
               , artistIsniCodes :: !(Set ISNI)
               , artistAnnotation :: !Text
               }
  treeData ArtistTree{..} = artistData

deriving instance Eq (Tree Artist)
deriving instance Show (Tree Artist)

instance TreeAliases Artist where
  aliases f artist =
    f (artistAliases artist) <&> \b -> artist { artistAliases = b }

instance TreeAnnotation Artist where
  annotation f artist =
    f (artistAnnotation artist) <&> \b -> artist { artistAnnotation = b }

instance TreeISNICodes Artist where
  isniCodes f artist =
    f (artistIsniCodes artist) <&> \b -> artist { artistIsniCodes = b }

instance TreeIPICodes Artist where
  ipiCodes f artist =
    f (artistIpiCodes artist) <&> \b -> artist { artistIpiCodes = b }

instance TreeRelationships Artist where
  relationships f artist =
    f (artistRelationships artist) <&> \b -> artist { artistRelationships = b }

instance Mergeable (Tree Artist) where
  type MergeRender (Tree Artist) mo =
    ( Render (Maybe (Ref ArtistType)) mo
    , Render (Maybe (Ref Country)) mo
    , Render (Maybe (Ref Gender)) mo
    , Render (Set.Set (Alias Artist)) mo
    , Render (Set.Set IPI) mo
    , Render (Set.Set ISNI) mo
    , Render (Set.Set LinkedRelationship) mo
    , Render Bool mo
    , Render PartialDate mo
    , Render Text mo
    )

  merge =
    ArtistTree <$> "Artist" .>
                     artistData `mergedVia` mergeArtistData
               <*> "Relationships" .>
                     artistRelationships `mergedVia` merge
               <*> "Aliases" .>
                     artistAliases `mergedVia` merge
               <*> "IPI Codes" .>
                     artistIpiCodes `mergedVia` merge
               <*> "ISNI Codes" .>
                     artistIsniCodes `mergedVia` merge
               <*> "Annotation" .>
                     artistAnnotation `mergedVia` mergeEq
    where
      mergeArtistData =
        Artist <$> artistName `mergedVia` mergeEq
               <*> artistSortName `mergedVia` mergeEq
               <*> artistComment `mergedVia` mergeEq
               <*> artistBeginDate `mergedVia` mergeEq
               <*> artistEndDate `mergedVia` mergeEq
               <*> artistEnded `mergedVia` mergeEq
               <*> artistGender `mergedVia` mergeEq
               <*> artistType `mergedVia` mergeEq
               <*> artistCountry `mergedVia` mergeEq

instance Referenceable Artist where
  type RefSpec Artist = MBID Artist

instance FromField (Ref Artist) where
  fromField f v = view reference <$> fromField f v

instance FromRow Artist where
  fromRow = Artist <$> field <*> field <*> field <*> fromRow
                   <*> fromRow <*> field <*> field <*> field
                   <*> field

instance ToField (Ref Artist) where
  toField = toField . dereference

instance ToRow Artist where
  toRow Artist{..} = [ toField artistName
                     , toField artistSortName
                     , toField artistComment
                     ]
                     ++ toRow artistBeginDate
                     ++ toRow artistEndDate
                     ++
                     [
                       toField artistEnded
                     , toField artistGender
                     , toField artistType
                     , toField artistCountry
                     ]

instance RootTable Artist where
  rootTable = Tagged "artist"

instance Create Artist

instance NewEntityRevision Artist

instance MasterRevision Artist

instance Editable Artist where

instance CloneRevision Artist

instance ResolveReference (Revision Artist)

instance ResolveReference Artist

instance Update Artist

instance ViewRevision Artist where
  viewRevision revision = head <$> query q (Only revision)
    where q = [sql|
       SELECT artist_id, revision_id,
        name.name, sort_name.name, comment,
        begin_date_year, begin_date_month, begin_date_day,
        end_date_year, end_date_month, end_date_day, ended,
        gender_id, artist_type_id, country_id
      FROM artist
      JOIN artist_revision USING (artist_id)
      JOIN artist_tree USING (artist_tree_id)
      JOIN artist_data USING (artist_data_id)
      JOIN artist_name name ON (artist_data.name = name.id)
      JOIN artist_name sort_name ON (artist_data.sort_name = sort_name.id)
      WHERE revision_id = ? |]

instance RealiseTree Artist where
  realiseTree artist = do
    dataId <- insertArtistData (artistData artist)
    treeId <- insertArtistTree (artistAnnotation artist) dataId

    Generic.realiseRelationships "artist" treeId artist
    Generic.realiseAliases "artist" treeId artist
    Generic.realiseIpiCodes "artist" treeId artist
    Generic.realiseIsniCodes "artist" treeId artist

    return treeId
    where
      insertArtistData data' = fmap (`asTypeOf` (1 :: Int)) <$> selectValue $
        query [sql| SELECT find_or_insert_artist_data(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) |]
          data'

      insertArtistTree annotationBody dataId = selectValue $
        query [sql| INSERT INTO artist_tree (artist_data_id, annotation)
                    VALUES (?, ?)
                    RETURNING artist_tree_id  |]
          (dataId, annotationBody)

instance FindLatest Artist where
  findLatest = Generic.findLatest
    [sql|
      SELECT artist_id, revision_id,
        name.name, sort_name.name, comment,
        begin_date_year, begin_date_month, begin_date_day,
        end_date_year, end_date_month, end_date_day, ended,
        gender_id, artist_type_id, country_id
      FROM artist
      JOIN artist_revision USING (artist_id)
      JOIN artist_tree USING (artist_tree_id)
      JOIN artist_data USING (artist_data_id)
      JOIN artist_name name ON (artist_data.name = name.id)
      JOIN artist_name sort_name ON (artist_data.sort_name = sort_name.id)
      WHERE artist_id IN ?
        AND revision_id = master_revision_id
    |]

instance HoldsRelationships Artist where
  reflectRelationshipChange = Generic.reflectRelationshipChange ArtistRelationship

instance ViewAliases Artist

instance ViewAnnotation Artist

instance ViewIPICodes Artist

instance ViewISNICodes Artist

instance ViewTree Artist where
  viewTree r = ArtistTree <$> fmap coreData (viewRevision r)
                          <*> viewRelationships r
                          <*> viewAliases r
                          <*> viewOnce viewIpiCodes r
                          <*> viewOnce viewIsniCodes r
                          <*> viewAnnotation r


--------------------------------------------------------------------------------
{-| The definition of a type of an artist (e.g., \"person\" or \"group\") . -}
newtype ArtistType = ArtistType { artistTypeName :: Text }
  deriving (Eq, Show)

instance Referenceable ArtistType where
  type RefSpec ArtistType = Int

instance FromField (Ref ArtistType) where
  fromField f v = view reference <$> fromField f v

instance FromRow ArtistType where
  fromRow = ArtistType <$> field

instance ToField (Ref ArtistType) where
  toField = toField . dereference

instance ToRow ArtistType where
  toRow ArtistType{..} = [ toField artistTypeName
                         ]

instance Add ArtistType where
  add artistType = head <$>
    query [sql| INSERT INTO artist_type (name) VALUES (?)
                RETURNING id, name |] artistType

instance ResolveReference ArtistType where
  resolveReference artistTypeId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM artist_type WHERE id = ? |] (Only artistTypeId)
