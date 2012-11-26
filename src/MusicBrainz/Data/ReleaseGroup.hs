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
import MusicBrainz.Data.Annotation
import MusicBrainz.Data.Create
import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.Merge
import MusicBrainz.Data.Revision.Internal
import MusicBrainz.Data.Tree
import MusicBrainz.Data.Update
import MusicBrainz.Edit

import qualified MusicBrainz.Data.Generic as Generic

--------------------------------------------------------------------------------
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
instance ViewRevision ReleaseGroup where
  viewRevision revision = head <$> query q (Only revision)
    where q = [sql|
       SELECT release_group_id, revision_id,
        name.name, comment, artist_credit_id, release_group_primary_type_id
      FROM release_group
      JOIN release_group_revision USING (release_group_id)
      JOIN release_group_tree USING (release_group_tree_id)
      JOIN release_group_data USING (release_group_data_id)
      JOIN release_name name ON (release_group_data.name = name.id)
      WHERE revision_id = ? |]


--------------------------------------------------------------------------------
instance Editable ReleaseGroup where
  linkRevisionToEdit = Generic.linkRevisionToEdit "edit_release_group"


--------------------------------------------------------------------------------
instance ViewTree ReleaseGroup where
  viewTree r = ReleaseGroupTree <$> fmap coreData (viewRevision r)
                                <*> viewAnnotation r


--------------------------------------------------------------------------------
instance Merge ReleaseGroup


 --------------------------------------------------------------------------------
instance Create ReleaseGroup where
  create = Generic.create "release_group"


--------------------------------------------------------------------------------
instance CloneRevision ReleaseGroup where
  cloneRevision = Generic.cloneRevision "release_group"


--------------------------------------------------------------------------------
instance RealiseTree ReleaseGroup where
  realiseTree rg = do
    dataId <- insertRgData (releaseGroupData rg)
    insertRgTree (releaseGroupAnnotation rg) dataId
    where
      insertRgData :: (Functor m, MonadIO m) => ReleaseGroup -> MusicBrainzT m Int
      insertRgData data' = selectValue $
        query [sql| SELECT find_or_insert_release_group_data(?, ?, ?, ?) |]
          data'

      insertRgTree annotation dataId = selectValue $
        query [sql| INSERT INTO release_group_tree (release_group_data_id, annotation)
                    VALUES (?, ?)
                    RETURNING release_group_tree_id  |]
          (dataId, annotation)


--------------------------------------------------------------------------------
instance NewEntityRevision ReleaseGroup where
  newEntityRevision revisionId rgId rgTreeId = void $
    execute [sql| INSERT INTO release_group_revision (release_group_id, revision_id, release_group_tree_id) VALUES (?, ?, ?) |]
      (rgId, revisionId, rgTreeId)


--------------------------------------------------------------------------------
instance MasterRevision ReleaseGroup where
  setMasterRevision = Generic.setMasterRevision "release_group"


--------------------------------------------------------------------------------
instance ViewAnnotation ReleaseGroup where
  viewAnnotation = Generic.viewAnnotation "release_group"


--------------------------------------------------------------------------------
instance Update ReleaseGroup where
  update editor baseRev releaseGroup = do
    treeId <- realiseTree releaseGroup
    revisionId <- newChildRevision editor baseRev treeId
    includeRevision revisionId
    return revisionId

