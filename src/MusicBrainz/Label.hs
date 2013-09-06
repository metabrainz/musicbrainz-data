{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module MusicBrainz.Label where

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

import MusicBrainz.Util (viewOnce)
import MusicBrainz.Merge
import MusicBrainz.Monad
import MusicBrainz.Alias
import MusicBrainz.Annotation
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
import MusicBrainz.Entity (Add(..), coreData)
import MusicBrainz.IPI
import MusicBrainz.ISNI
import MusicBrainz.MBID (MBID)
import MusicBrainz.PartialDate (PartialDate)
import MusicBrainz.Ref (Ref, Referenceable(..), reference, dereference)
import MusicBrainz.Relationship
import MusicBrainz.Relationship.Internal (HoldsRelationships(..), viewRelationships)
import MusicBrainz.Revision (Revision)
import MusicBrainz.Revision.Internal (CloneRevision(..))
import MusicBrainz.Tree

import {-# SOURCE #-} qualified MusicBrainz.Generic as Generic

--------------------------------------------------------------------------------
{-| A label who is repsonsible for releasing/distributing music. -}
data Label = Label { labelName :: !Text
                   , labelSortName :: !Text
                   , labelComment :: !Text
                   , labelBeginDate :: !PartialDate
                   , labelEndDate :: !PartialDate
                   , labelEnded :: !Bool
                   , labelType :: !(Maybe (Ref LabelType))
                   , labelCode :: !(Maybe Int)
                   , labelCountry :: !(Maybe (Ref Country))
                   }
  deriving (Eq, Show)

instance Referenceable Label where
  type RefSpec Label = MBID Label

instance FromField (Ref Label) where
  fromField f v = view reference <$> fromField f v

instance FromRow Label where
  fromRow = Label <$> field <*> field <*> field <*> fromRow <*> fromRow
                  <*> field <*> field <*> field <*> field

instance ToField (Ref Label) where
  toField = toField . dereference

instance ToRow Label where
  toRow Label{..} = [ toField labelName
                    , toField labelSortName
                    , toField labelComment
                    ]
                    ++ toRow labelBeginDate
                    ++ toRow labelEndDate
                    ++
                    [
                      toField labelEnded
                    , toField labelType
                    , toField labelCode
                    , toField labelCountry
                    ]

instance HasTree Label where
  data Tree Label =
    LabelTree { labelData :: !Label
              , labelRelationships :: !(Set LinkedRelationship)
              , labelAliases :: !(Set (Alias Label))
              , labelIpiCodes :: !(Set IPI)
              , labelIsniCodes :: !(Set ISNI)
              , labelAnnotation :: !Text
              }

  treeData LabelTree{..} = labelData

deriving instance Eq (Tree Label)
deriving instance Show (Tree Label)

instance TreeAliases Label where
  aliases f label = f (labelAliases label) <&> \b -> label { labelAliases = b }

instance TreeAnnotation Label where
  annotation f label = f (labelAnnotation label) <&> \b -> label { labelAnnotation = b }

instance TreeIPICodes Label where
  ipiCodes f label = f (labelIpiCodes label) <&> \b -> label { labelIpiCodes = b }

instance TreeISNICodes Label where
  isniCodes f label = f (labelIsniCodes label) <&> \b -> label { labelIsniCodes = b }

instance TreeRelationships Label where
  relationships f label = f (labelRelationships label) <&> \b -> label { labelRelationships = b }

instance Mergeable (Tree Label) where
  type MergeRender (Tree Label) mo =
    ( Render (Maybe Int) mo
    , Render (Maybe (Ref LabelType)) mo
    , Render (Maybe (Ref Country)) mo
    , Render (Set.Set (Alias Label)) mo
    , Render (Set.Set IPI) mo
    , Render (Set.Set ISNI) mo
    , Render (Set.Set LinkedRelationship) mo
    , Render Bool mo
    , Render PartialDate mo
    , Render Text mo
    )

  merge =
    LabelTree <$> labelData `mergedVia` mergeLabelData
              <*> labelRelationships `mergedVia` merge
              <*> labelAliases `mergedVia` merge
              <*> labelIpiCodes `mergedVia` merge
              <*> labelIsniCodes `mergedVia` merge
              <*> labelAnnotation `mergedVia` mergeEq
    where
      mergeLabelData =
        Label <$> labelName `mergedVia` mergeEq
              <*> labelSortName `mergedVia` mergeEq
              <*> labelComment `mergedVia` mergeEq
              <*> labelBeginDate `mergedVia` mergeEq
              <*> labelEndDate `mergedVia` mergeEq
              <*> labelEnded `mergedVia` mergeEq
              <*> labelType `mergedVia` mergeEq
              <*> labelCode `mergedVia` mergeEq
              <*> labelCountry `mergedVia` mergeEq

instance CloneRevision Label

instance Create Label

instance MasterRevision Label

instance NewEntityRevision Label

instance ResolveReference (Revision Label)

instance ResolveReference Label

instance Update Label

instance ViewAliases Label

instance ViewAnnotation Label

instance ViewIPICodes Label

instance ViewISNICodes Label

instance RootTable Label where
  rootTable = Tagged "label"

instance HoldsRelationships Label where
  reflectRelationshipChange = Generic.reflectRelationshipChange LabelRelationship

instance FindLatest Label where
  findLatest = Generic.findLatest
    [sql|
      SELECT label_id, revision_id,
        name.name, sort_name.name, comment,
        begin_date_year, begin_date_month, begin_date_day,
        end_date_year, end_date_month, end_date_day,
        ended, label_type_id, label_code, country_id
      FROM label
      JOIN label_revision USING (label_id)
      JOIN label_tree USING (label_tree_id)
      JOIN label_data USING (label_data_id)
      JOIN label_name name ON (label_data.name = name.id)
      JOIN label_name sort_name ON (label_data.sort_name = sort_name.id)
      WHERE label_id IN ?
        AND revision_id = master_revision_id  |]

instance RealiseTree Label where
  realiseTree label = do
    dataId <- insertLabelData (labelData label)
    treeId <- insertLabelTree (labelAnnotation label) dataId

    Generic.realiseRelationships "label" treeId label
    Generic.realiseAliases "label" treeId label
    Generic.realiseIpiCodes "label" treeId label
    Generic.realiseIsniCodes "label" treeId label

    return treeId
    where
      insertLabelData data' = fmap (`asTypeOf` (1 :: Int)) <$> selectValue $
        query [sql| SELECT find_or_insert_label_data(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) |]
          data'

      insertLabelTree annotationBody dataId = selectValue $
        query [sql| INSERT INTO label_tree (label_data_id, annotation)
                    VALUES (?, ?)
                    RETURNING label_tree_id  |]
          (dataId, annotationBody)

instance ViewRevision Label where
  viewRevision revisionId = head <$> query q (Only revisionId)
    where q = [sql|
       SELECT label_id, revision_id,
        name.name, sort_name.name, comment,
        begin_date_year, begin_date_month, begin_date_day,
        end_date_year, end_date_month, end_date_day,
        ended, label_type_id, label_code, country_id
      FROM label
      JOIN label_revision USING (label_id)
      JOIN label_tree USING (label_tree_id)
      JOIN label_data USING (label_data_id)
      JOIN label_name name ON (label_data.name = name.id)
      JOIN label_name sort_name ON (label_data.sort_name = sort_name.id)
      WHERE revision_id = ? |]

instance ViewTree Label where
  viewTree r = LabelTree <$> fmap coreData (viewRevision r)
                         <*> viewRelationships r
                         <*> viewAliases r
                         <*> viewOnce viewIpiCodes r
                         <*> viewOnce viewIsniCodes r
                         <*> viewAnnotation r

instance Editable Label


--------------------------------------------------------------------------------
{-| The definition of a type of an label (e.g., \"person\" or \"group\") . -}
newtype LabelType = LabelType { labelTypeName :: Text }
  deriving (Eq, Show)

instance Referenceable LabelType where
  type RefSpec LabelType = Int

instance FromField (Ref LabelType) where
  fromField f v = view reference <$> fromField f v

instance FromRow LabelType where
  fromRow = LabelType <$> field

instance ToField (Ref LabelType) where
  toField = toField . dereference

instance ToRow LabelType where
  toRow LabelType{..} = [ toField labelTypeName
                        ]

instance Add LabelType where
  add labelType = head <$>
    query [sql| INSERT INTO label_type (name) VALUES (?)
                RETURNING id, name |] labelType

instance ResolveReference LabelType where
  resolveReference labelTypeId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM label_type WHERE id = ? |] (Only labelTypeId)
