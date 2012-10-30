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
import MusicBrainz.Data.Revision

instance FindLatest Label where
  findLatest mbid = listToMaybe <$> query q (Only mbid)
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
create editor label = do
  labelTreeId <- labelTree label
  labelId <- reserveLabel
  revisionId <- newRevision editor >>= newLabelRevision labelId labelTreeId
  linkRevision labelId revisionId
  return CoreEntity { coreMbid = labelId
                    , coreRevision = revisionId
                    , coreData = label
                    }
  where
    reserveLabel :: MusicBrainz (MBID Label)
    reserveLabel = selectValue $
      query_ [sql| INSERT INTO label (master_revision_id) VALUES (-1) RETURNING label_id |]

    newLabelRevision :: MBID Label -> Int -> Ref (Revision Label) -> MusicBrainz (Ref (Revision Label))
    newLabelRevision labelId labelTreeId revisionId = selectValue $
      query [sql| INSERT INTO label_revision (label_id, revision_id, label_tree_id)
                  VALUES (?, ?, ?) RETURNING revision_id |]
        (labelId, revisionId, labelTreeId)


    linkRevision :: MBID Label -> Ref (Revision Label) -> MusicBrainz ()
    linkRevision labelId revisionId = void $
      execute [sql| UPDATE label SET master_revision_id = ? WHERE label_id = ? |] (revisionId, labelId)

labelTree :: Label -> MusicBrainz Int
labelTree label = findOrInsertLabelData >>= findOrInsertLabelTree
  where
    findOrInsertLabelData :: MusicBrainz Int
    findOrInsertLabelData = selectValue $
      query [sql| SELECT find_or_insert_label_data(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) |]
        label

    findOrInsertLabelTree dataId = selectValue $
      query [sql| SELECT find_or_insert_label_tree(?) |]
        (Only dataId)

