{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Functions for interacting with 'ReleaseGroup's in the MusicBrainz database.

The majority of operations on releases are common for all core entities, so you
should see the documentation on the 'Release' type and notice all the type class
instances. -}
module MusicBrainz.Data.ReleaseGroup
    ( ) where

import Control.Applicative
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz
import MusicBrainz.Data.Create
import MusicBrainz.Data.FindLatest

import qualified MusicBrainz.Data.Generic.Create as GenericCreate

instance FindLatest ReleaseGroup where
  findLatest releaseGroupId = head <$> query q (Only releaseGroupId)
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
instance Create ReleaseGroup where
  create = GenericCreate.create GenericCreate.Specification
      { GenericCreate.getTree = findOrInsertRgTree
      , GenericCreate.reserveEntity = GenericCreate.reserveEntityTable "release_group"
      , GenericCreate.newEntityRevision = newRgRevision
      , GenericCreate.linkRevision = linkRevision
      }
    where
      findOrInsertRgData :: (Functor m, MonadIO m) => Tree ReleaseGroup -> MusicBrainzT m Int
      findOrInsertRgData rg = selectValue $
        query [sql| SELECT find_or_insert_release_group_data(?, ?, ?, ?) |]
          (treeData rg)

      findOrInsertRgTree rg = do
        dataId <- findOrInsertRgData rg
        selectValue $
          query [sql| SELECT find_or_insert_release_group_tree(?) |]
            (Only dataId)

      newRgRevision rgId rgTreeId revisionId = selectValue $
        query [sql| INSERT INTO release_group_revision (release_group_id, revision_id, release_group_tree_id) VALUES (?, ?, ?) RETURNING revision_id |]
          (rgId, revisionId, rgTreeId)

      linkRevision rgId revisionId = void $
        execute [sql| UPDATE release_group SET master_revision_id = ? WHERE release_group_id = ? |] (revisionId, rgId)
