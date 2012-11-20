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
import MusicBrainz.Edit

import qualified MusicBrainz.Data.Generic.Create as GenericCreate
import qualified MusicBrainz.Data.Generic.Revision as GenericRevision

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
      { GenericCreate.reserveEntity = GenericCreate.reserveEntityTable "release_group"
      }


--------------------------------------------------------------------------------
instance RealiseTree ReleaseGroup where
  realiseTree rg = do
    dataId <- findOrInsertRgData
    selectValue $
      query [sql| SELECT find_or_insert_release_group_tree(?) |]
        (Only dataId)
    where
      findOrInsertRgData :: (Functor m, MonadIO m) => MusicBrainzT m Int
      findOrInsertRgData = selectValue $
        query [sql| SELECT find_or_insert_release_group_data(?, ?, ?, ?) |]
          (treeData rg)


--------------------------------------------------------------------------------
instance NewEntityRevision ReleaseGroup where
  newEntityRevision revisionId rgId rgTreeId = void $
    execute [sql| INSERT INTO release_group_revision (release_group_id, revision_id, release_group_tree_id) VALUES (?, ?, ?) |]
      (rgId, revisionId, rgTreeId)


--------------------------------------------------------------------------------
instance MasterRevision ReleaseGroup where
  setMasterRevision = GenericRevision.setMasterRevision "release_group"
