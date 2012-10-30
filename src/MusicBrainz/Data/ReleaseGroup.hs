{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Functions for interacting with 'ReleaseGroup's in the MusicBrainz database. -}
module MusicBrainz.Data.ReleaseGroup
    ( create ) where

import Control.Applicative
import Control.Monad (void)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz
import MusicBrainz.Data.FindLatest

instance FindLatest ReleaseGroup where
  findLatest mbid = listToMaybe <$> query q (Only mbid)
    where q = [sql|
       SELECT release_group_id, revision_id,
         name.name, comment, artist_credit_id, release_group_primary_type_id
      FROM release_group
      JOIN release_group_revision USING (release_group_id)
      JOIN release_group_tree USING (release_group_tree_id)
      JOIN release_group_data USING (release_group_data_id)
      JOIN release_name name ON (release_group_data.name = name.id)
      WHERE release_group_id = ?
        AND revision_id = master_revision_id  |]

--------------------------------------------------------------------------------
{-| Create an entirely new release group, returning the final 'CoreEntity' as it
is in the database. -}
create :: Ref Editor -> ReleaseGroup -> MusicBrainz (CoreEntity ReleaseGroup)
create editor rg = do
  rgTreeId <- findOrInsertRgData >>= findOrInsertRgTree
  rgId <- reserveRg
  revisionId <- newRevision >>= newRgRevision rgId rgTreeId
  linkRevision rgId revisionId
  return CoreEntity { coreMbid = rgId
                    , coreRevision = revisionId
                    , coreData = rg
                    }
  where
    selectId = fmap (fromOnly . head)

    findOrInsertRgData :: MusicBrainz Int
    findOrInsertRgData = selectId $
      query [sql| SELECT find_or_insert_release_group_data(?, ?, ?, ?) |]
        rg

    findOrInsertRgTree dataId = selectId $
      query [sql| SELECT find_or_insert_release_group_tree(?) |]
        (Only dataId)

    newRevision :: MusicBrainz (Ref (Revision ReleaseGroup))
    newRevision = selectId $
      query [sql| INSERT INTO revision (editor_id) VALUES (?) RETURNING revision_id |]
        (Only editor)

    reserveRg :: MusicBrainz (MBID ReleaseGroup)
    reserveRg = selectId $
      query_ [sql| INSERT INTO release_group (master_revision_id) VALUES (-1) RETURNING release_group_id |]

    newRgRevision :: MBID ReleaseGroup -> Int -> Ref (Revision ReleaseGroup) -> MusicBrainz (Ref (Revision ReleaseGroup))
    newRgRevision rgId rgTreeId revisionId = selectId $
      query [sql| INSERT INTO release_group_revision (release_group_id, revision_id, release_group_tree_id) VALUES (?, ?, ?) RETURNING revision_id |]
        (rgId, revisionId, rgTreeId)

    linkRevision :: MBID ReleaseGroup -> Ref (Revision ReleaseGroup) -> MusicBrainz ()
    linkRevision rgId revisionId = void $
      execute [sql| UPDATE release_group SET master_revision_id = ? WHERE release_group_id = ? |] (revisionId, rgId)
