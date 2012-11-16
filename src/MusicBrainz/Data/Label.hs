{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions for interacting with MusicBrainz labels in the database.

The majority of operations on labels are common for all core entities, so you
should see the documentation on the 'Label' type and notice all the type class
instances. -}
module MusicBrainz.Data.Label
    ( ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ

import MusicBrainz
import MusicBrainz.Data.Create
import MusicBrainz.Data.FindLatest

import qualified MusicBrainz.Data.Generic.Create as GenericCreate

instance FindLatest Label where
  findLatest labelId = head <$> query q (Only labelId)
    where q = [sql|
       SELECT label_id, revision_id,
        name.name, sort_name.name, comment,
        begin_date_year, begin_date_month, begin_date_day,
        end_date_year, end_date_month, end_date_day,
        ended, label_type_id, label_code
      FROM label
      JOIN label_revision USING (label_id)
      JOIN label_tree USING (label_tree_id)
      JOIN label_data USING (label_data_id)
      JOIN label_name name ON (label_data.name = name.id)
      JOIN label_name sort_name ON (label_data.sort_name = sort_name.id)
      WHERE label_id = ?
        AND revision_id = master_revision_id  |]


--------------------------------------------------------------------------------
instance Create Label where
  create = GenericCreate.create GenericCreate.Specification
      { GenericCreate.getTree = labelTree
      , GenericCreate.reserveEntity = GenericCreate.reserveEntityTable "label"
      , GenericCreate.newEntityRevision = newLabelRevision
      , GenericCreate.linkRevision = linkRevision
      }
    where
      newLabelRevision labelId labelTreeId revisionId = selectValue $
        query [sql| INSERT INTO label_revision (label_id, revision_id, label_tree_id)
                    VALUES (?, ?, ?) RETURNING revision_id |]
          (labelId, revisionId, labelTreeId)

      linkRevision labelId revisionId = void $
        execute [sql| UPDATE label SET master_revision_id = ? WHERE label_id = ? |] (revisionId, labelId)


--------------------------------------------------------------------------------
labelTree :: (Functor m, MonadIO m) => Tree Label -> MusicBrainzT m (Ref (Tree Label))
labelTree label = findOrInsertLabelData >>= findOrInsertLabelTree
  where
    findOrInsertLabelData :: (Functor m, MonadIO m) => MusicBrainzT m Int
    findOrInsertLabelData = selectValue $
      query [sql| SELECT find_or_insert_label_data(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) |]
        (treeData label)

    findOrInsertLabelTree dataId = selectValue $
      query [sql| SELECT find_or_insert_label_tree(?) |]
        (Only dataId)

