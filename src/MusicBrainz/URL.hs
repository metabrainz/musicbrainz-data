{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module MusicBrainz.URL where

import Control.Applicative
import Control.Lens hiding ((.>))
import Data.ByteString (ByteString)
import Data.Set (Set)
import Data.Tagged (Tagged(..))
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), fieldWith)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.ToRow (ToRow(..))
import Network.URI (URI, parseURI)

import qualified Database.PostgreSQL.Simple.FromField as FF
import qualified Data.Set as Set

import MusicBrainz.Merge
import MusicBrainz.Monad
import MusicBrainz.Class.RootTable
import MusicBrainz.Class.Update
import MusicBrainz.MBID (MBID)
import MusicBrainz.Relationship
import MusicBrainz.Relationship.Internal
import MusicBrainz.Versioning hiding (merge)

import {-# SOURCE #-} qualified MusicBrainz.Generic as Generic

--------------------------------------------------------------------------------
newtype URL = URL { urlUrl :: URI }
  deriving (Eq, Show)

instance Referenceable URL where
  type RefSpec URL = MBID URL

instance FromField (Ref URL) where
  fromField f v = view reference <$> fromField f v

instance FromRow URL where
  fromRow = URL <$> fieldWith uriFromField

instance ToField (Ref URL) where
  toField = toField . dereference

instance ToRow URL where
  toRow URL{..} = [ toField (show urlUrl) ]

instance HasTree URL where
  data Tree URL =
    URLTree { urlData :: !URL
            , urlRelationships :: !(Set LinkedRelationship)
            }

  treeData URLTree{..} = urlData

deriving instance Eq (Tree URL)
deriving instance Show (Tree URL)

instance TreeRelationships URL where
  relationships f url =
    f (urlRelationships url) <&> \b -> url { urlRelationships = b }

instance RootTable URL where
  rootTable = Tagged "URL"

instance HoldsRelationships URL where
  reflectRelationshipChange = Generic.reflectRelationshipChange URLRelationship

instance Mergeable (Tree URL) where
  type MergeRender (Tree URL) mo =
    ( Render (Set.Set LinkedRelationship) mo
    , Render URI mo
    )

  merge =
    URLTree <$> urlData `mergedVia` mergeUrlData
            <*> urlRelationships `mergedVia` merge
    where
      mergeUrlData =
        URL <$> urlUrl `mergedVia` mergeEq

instance Editable URL

instance RealiseTree URL where
  realiseTree url = do
    dataId <- insertURLData (urlData url)
    treeId <- insertURLTree dataId
    Generic.realiseRelationships "URL" treeId url
    return treeId
    where
      insertURLData data' = fmap (`asTypeOf` (1 :: Int)) <$> selectValue $
        query [sql| SELECT find_or_insert_URL_data(?) |]
          data'

      insertURLTree dataId = selectValue $
        query [sql| INSERT INTO URL_tree (URL_data_id)
                    VALUES (?)
                    RETURNING URL_tree_id  |]
          (Only dataId)

instance ViewRevision URL where
  viewRevision revision = head <$> query q (Only revision)
    where q = [sql|
       SELECT URL_id, revision_id, URL
      FROM URL
      JOIN URL_revision USING (URL_id)
      JOIN URL_tree USING (URL_tree_id)
      JOIN URL_data USING (URL_data_id)
      WHERE revision_id = ? |]

instance ViewTree URL where
  viewTree r = URLTree <$> fmap coreData (viewRevision r)
                       <*> viewRelationships r

instance FindLatest URL where
  findLatest = Generic.findLatest
    [sql|
      SELECT URL_id, revision_id, URL
      FROM URL
      JOIN URL_revision USING (URL_id)
      JOIN URL_tree USING (URL_tree_id)
      JOIN URL_data USING (URL_data_id)
      WHERE URL_id IN ?
        AND revision_id = master_revision_id  |]

instance CloneRevision URL

instance Create URL

instance Update URL

instance ResolveReference URL

instance ResolveReference (Revision URL)

instance MasterRevision URL

instance NewEntityRevision URL

--------------------------------------------------------------------------------
uriFromField :: FF.Field -> Maybe ByteString -> FF.Conversion URI
uriFromField f v = do
  attempt <- parseURI <$> fromField f v
  maybe
    (FF.returnError FF.ConversionFailed f "Failed to convert string to URI")
    return
    attempt
