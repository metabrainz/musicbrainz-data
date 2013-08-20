{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MusicBrainz.Data.Url () where

import Control.Applicative
import Control.Lens (prism)
import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Data.Tagged (Tagged(..))

import MusicBrainz
import MusicBrainz.Data.CoreEntity
import MusicBrainz.Data.Create
import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.Merge
import MusicBrainz.Data.Relationship
import MusicBrainz.Data.Relationship.Internal
import MusicBrainz.Data.Revision.Internal
import MusicBrainz.Data.Tree
import MusicBrainz.Data.Update
import MusicBrainz.Edit

import qualified MusicBrainz.Data.Generic as Generic

instance Merge Url
instance CloneRevision Url
instance Create Url
instance Update Url
instance ResolveReference Url
instance ResolveReference (Revision Url)
instance MasterRevision Url
instance NewEntityRevision Url

--------------------------------------------------------------------------------
instance CoreEntityTable Url where
  rootTable = Tagged "url"


--------------------------------------------------------------------------------
instance HoldsRelationships Url where
  reflectRelationshipChange = Generic.reflectRelationshipChange UrlRelationship


--------------------------------------------------------------------------------
instance Editable Url where
  change = prism UrlChange extract
    where extract a = case a of UrlChange c -> Right c
                                _ -> Left a


--------------------------------------------------------------------------------
instance RealiseTree Url where
  realiseTree url = do
    dataId <- insertUrlData (urlData url)
    treeId <- insertUrlTree dataId
    Generic.realiseRelationships "url" treeId url
    return treeId
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
                       <*> viewRelationships r


--------------------------------------------------------------------------------
instance FindLatest Url where
  findLatest = Generic.findLatest
    [sql|
      SELECT url_id, revision_id, url
      FROM url
      JOIN url_revision USING (url_id)
      JOIN url_tree USING (url_tree_id)
      JOIN url_data USING (url_data_id)
      WHERE url_id IN ?
        AND revision_id = master_revision_id  |]


