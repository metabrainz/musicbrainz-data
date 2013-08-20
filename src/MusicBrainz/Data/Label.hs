{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions for interacting with MusicBrainz labels in the database.

The majority of operations on labels are common for all core entities, so you
should see the documentation on the 'Label' type and notice all the type class
instances. -}
module MusicBrainz.Data.Label
    ( ) where

import Control.Applicative
import Control.Lens (prism)
import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ
import Data.Tagged

import MusicBrainz
import MusicBrainz.Data.Alias
import MusicBrainz.Data.Annotation
import MusicBrainz.Data.CoreEntity
import MusicBrainz.Data.Create
import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.IPI
import MusicBrainz.Data.ISNI
import MusicBrainz.Data.Merge
import MusicBrainz.Data.Relationship
import MusicBrainz.Data.Relationship.Internal
import MusicBrainz.Data.Revision.Internal
import MusicBrainz.Data.Tree
import MusicBrainz.Data.Update
import MusicBrainz.Data.Util (viewOnce)
import MusicBrainz.Edit

import qualified MusicBrainz.Data.Generic as Generic

--------------------------------------------------------------------------------
instance CloneRevision Label
instance Create Label
instance MasterRevision Label
instance Merge Label
instance NewEntityRevision Label
instance ResolveReference (Revision Label)
instance ResolveReference Label
instance Update Label
instance ViewAliases Label
instance ViewAnnotation Label
instance ViewIPICodes Label
instance ViewISNICodes Label


--------------------------------------------------------------------------------
instance CoreEntityTable Label where
  rootTable = Tagged "label"


--------------------------------------------------------------------------------
instance HoldsRelationships Label where
  reflectRelationshipChange = Generic.reflectRelationshipChange LabelRelationship


--------------------------------------------------------------------------------
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


--------------------------------------------------------------------------------
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
      insertLabelData :: (Functor m, MonadIO m) => Label -> MusicBrainzT m Int
      insertLabelData data' = selectValue $
        query [sql| SELECT find_or_insert_label_data(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) |]
          data'

      insertLabelTree annotation dataId = selectValue $
        query [sql| INSERT INTO label_tree (label_data_id, annotation)
                    VALUES (?, ?)
                    RETURNING label_tree_id  |]
          (dataId, annotation)


--------------------------------------------------------------------------------
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


--------------------------------------------------------------------------------
instance ViewTree Label where
  viewTree r = LabelTree <$> fmap coreData (viewRevision r)
                         <*> viewRelationships r
                         <*> viewAliases r
                         <*> viewOnce viewIpiCodes r
                         <*> viewOnce viewIsniCodes r
                         <*> viewAnnotation r


--------------------------------------------------------------------------------
instance Editable Label where
  change = prism LabelChange extract
    where extract a = case a of LabelChange c -> Right c
                                _ -> Left a


