{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MusicBrainz.Data.Url () where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz
import MusicBrainz.Data.Create
import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.Merge
import MusicBrainz.Data.Revision.Internal
import MusicBrainz.Data.Tree
import MusicBrainz.Data.Update
import MusicBrainz.Edit

import qualified MusicBrainz.Data.Generic as Generic

--------------------------------------------------------------------------------
instance Editable Url where
  linkRevisionToEdit = Generic.linkRevisionToEdit "edit_url"

--------------------------------------------------------------------------------
instance MasterRevision Url where
  setMasterRevision = Generic.setMasterRevision "url"


--------------------------------------------------------------------------------
instance NewEntityRevision Url where
  newEntityRevision = Generic.newEntityRevision "url"


--------------------------------------------------------------------------------
instance RealiseTree Url where
  realiseTree url = insertUrlData (urlData url) >>= insertUrlTree
    where
      insertUrlData :: (Functor m, MonadIO m) => Url -> MusicBrainzT m Int
      insertUrlData data' = selectValue $
        query [sql| SELECT find_or_insert_url_data(?) |]
          data'

      insertUrlTree dataId = selectValue $
        query [sql| INSERT INTO url_tree (url_data_id)
                    VALUES (?)
                    RETURNING url_tree_id  |]
          (Only dataId)


--------------------------------------------------------------------------------
instance ViewRevision Url where
  viewRevision revision = head <$> query q (Only revision)
    where q = [sql|
       SELECT url_id, revision_id, url
      FROM url
      JOIN url_revision USING (url_id)
      JOIN url_tree USING (url_tree_id)
      JOIN url_data USING (url_data_id)
      WHERE revision_id = ? |]


--------------------------------------------------------------------------------
instance ViewTree Url where
  viewTree r = UrlTree <$> fmap coreData (viewRevision r)


--------------------------------------------------------------------------------
instance FindLatest Url where
  findLatest urlId = head <$> query q (Only urlId)
    where q = [sql|
       SELECT url_id, revision_id, url
      FROM url
      JOIN url_revision USING (url_id)
      JOIN url_tree USING (url_tree_id)
      JOIN url_data USING (url_data_id)
      WHERE url_id = ?
        AND revision_id = master_revision_id  |]


--------------------------------------------------------------------------------
instance Merge Url


--------------------------------------------------------------------------------
instance CloneRevision Url where
  cloneRevision = Generic.cloneRevision "url"


--------------------------------------------------------------------------------
instance Create Url where
  create = Generic.create "url"


--------------------------------------------------------------------------------
instance Update Url where
  update editor baseRev url = do
    treeId <- realiseTree url
    revisionId <- newChildRevision editor baseRev treeId
    includeRevision revisionId
    return revisionId

--------------------------------------------------------------------------------
instance ResolveReference Url where
  resolveReference = Generic.resolveMbid "url"
