{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions for interacting with MusicBrainz labels in the database. -}
module MusicBrainz.Data.Label
    ( create
    ) where

import Control.Applicative
import Control.Monad
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ

import MusicBrainz
import MusicBrainz.Data.FindLatest

import qualified MusicBrainz.Data.Generic.Create as GenericCreate

instance FindLatest Label where
  findLatest labelId = listToMaybe <$> query q (Only labelId)
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
{-| Create an entirely new 'Label', returning the final 'CoreEntity' as it is
in the database. -}
create :: Ref Editor -> Label -> MusicBrainz (CoreEntity Label)
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

labelTree :: Label -> MusicBrainz (Ref (Tree Label))
labelTree label = findOrInsertLabelData >>= findOrInsertLabelTree
  where
    findOrInsertLabelData :: MusicBrainz Int
    findOrInsertLabelData = selectValue $
      query [sql| SELECT find_or_insert_label_data(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) |]
        label

    findOrInsertLabelTree dataId = selectValue $
      query [sql| SELECT find_or_insert_label_tree(?) |]
        (Only dataId)

