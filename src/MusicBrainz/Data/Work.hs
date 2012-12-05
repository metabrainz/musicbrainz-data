{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions for interacting with MusicBrainz works in the database.

The majority of operations on works are common for all core entities, so you
should see the documentation on the 'Work' type and notice all the type class
instances. -}
module MusicBrainz.Data.Work
    ( ) where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ

import MusicBrainz
import MusicBrainz.Data.Alias
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
instance FindLatest Work where
  findLatest workId = head <$> query q (Only workId)
    where q = [sql|
       SELECT work_id, revision_id,
        name.name, comment, work_type_id, language_id
      FROM work
      JOIN work_revision USING (work_id)
      JOIN work_tree USING (work_tree_id)
      JOIN work_data USING (work_data_id)
      JOIN work_name name ON (work_data.name = name.id)
      WHERE work_id = ?
        AND revision_id = master_revision_id  |]


--------------------------------------------------------------------------------
instance Create Work where
  create = Generic.create "work"


--------------------------------------------------------------------------------
instance NewEntityRevision Work where
  newEntityRevision = Generic.newEntityRevision "work"


--------------------------------------------------------------------------------
instance MasterRevision Work where
  setMasterRevision = Generic.setMasterRevision "work"


--------------------------------------------------------------------------------
instance RealiseTree Work where
  realiseTree work = do
    dataId <- insertWorkData (workData work)
    treeId <- insertWorkTree (workAnnotation work) dataId
    Generic.realiseAliases "work" treeId work
    return treeId
    where
      insertWorkData :: (Functor m, MonadIO m) => Work -> MusicBrainzT m Int
      insertWorkData data' = selectValue $
        query [sql| SELECT find_or_insert_work_data(?, ?, ?, ?) |]
          data'

      insertWorkTree annotation dataId = selectValue $
        query [sql| INSERT INTO work_tree (work_data_id, annotation)
                    VALUES (?, ?)
                    RETURNING work_tree_id  |]
          (dataId, annotation)


--------------------------------------------------------------------------------
instance ViewRevision Work where
  viewRevision revisionId = head <$> query q (Only revisionId)
    where q = [sql|
       SELECT work_id, revision_id,
         name.name, comment, work_type_id, language_id
       FROM work
       JOIN work_revision USING (work_id)
       JOIN work_tree USING (work_tree_id)
       JOIN work_data USING (work_data_id)
       JOIN work_name name ON (work_data.name = name.id)
       WHERE revision_id = ? |]


--------------------------------------------------------------------------------
instance ViewTree Work where
  viewTree r = WorkTree <$> fmap coreData (viewRevision r)
                        <*> viewAliases r
                        <*> viewAnnotation r


--------------------------------------------------------------------------------
instance Editable Work where
  linkRevisionToEdit = Generic.linkRevisionToEdit "edit_work"


--------------------------------------------------------------------------------
instance ViewAliases Work where
  viewAliases = Generic.viewAliases "work"


--------------------------------------------------------------------------------
instance ViewAnnotation Work where
  viewAnnotation = Generic.viewAnnotation "work"


--------------------------------------------------------------------------------
instance CloneRevision Work where
  cloneRevision = Generic.cloneRevision "work"


--------------------------------------------------------------------------------
instance Update Work where
  update editor baseRev work = do
    revisionId <- runUpdate work baseRev
    return revisionId
    where
      runUpdate tree base = do
        treeId <- realiseTree tree
        revisionId <- newChildRevision editor base treeId
        includeRevision revisionId
        return revisionId


--------------------------------------------------------------------------------
instance ResolveReference Work where
  resolveReference = Generic.resolveMbid "work"


--------------------------------------------------------------------------------
instance Merge Work
