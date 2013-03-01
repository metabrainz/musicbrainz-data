{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions for interacting with MusicBrainz artists in the database.

The majority of operations on artists are common for all core entities, so you
should see the documentation on the 'Artist' type and notice all the type class
instances. -}
module MusicBrainz.Data.Artist () where

import Control.Applicative
import Control.Lens (prism)
import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ

import MusicBrainz
import MusicBrainz.Data.Alias
import MusicBrainz.Data.Annotation
import MusicBrainz.Data.Create
import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.IPI
import MusicBrainz.Data.Merge
import MusicBrainz.Data.Relationship
import MusicBrainz.Data.Relationship.Internal
import MusicBrainz.Data.Revision.Internal
import MusicBrainz.Data.Tree
import MusicBrainz.Data.Update
import MusicBrainz.Data.Util (viewOnce)
import MusicBrainz.Edit

import qualified MusicBrainz.Data.Generic as Generic

--------------------------------------------------------------------------------
instance HoldsRelationships Artist where
  fetchEndPoints = Generic.fetchEndPoints "artist"
  reflectRelationshipChange = Generic.reflectRelationshipChange ArtistRelationship


--------------------------------------------------------------------------------
instance ViewTree Artist where
  viewTree r = ArtistTree <$> fmap coreData (viewRevision r)
                          <*> viewRelationships r
                          <*> viewAliases r
                          <*> viewOnce viewIpiCodes r
                          <*> viewAnnotation r


--------------------------------------------------------------------------------
instance ViewAliases Artist where
  viewAliases = Generic.viewAliases "artist"


--------------------------------------------------------------------------------
instance ViewIPICodes Artist where
  viewIpiCodes = Generic.viewIpiCodes "artist"


--------------------------------------------------------------------------------
{-| View the annotation for a specific revision of an 'Artist'. -}
instance ViewAnnotation Artist where
  viewAnnotation = Generic.viewAnnotation "artist"


--------------------------------------------------------------------------------
instance Editable Artist where
  linkRevisionToEdit = Generic.linkRevisionToEdit "edit_artist"

  change = prism ArtistChange extract
    where extract a = case a of ArtistChange c -> Right c
                                _ -> Left a


--------------------------------------------------------------------------------
instance MasterRevision Artist where
  setMasterRevision = Generic.setMasterRevision "artist"


--------------------------------------------------------------------------------
instance FindLatest Artist where
  findLatest = Generic.findLatest
    [sql|
      SELECT artist_id, revision_id,
        name.name, sort_name.name, comment,
        begin_date_year, begin_date_month, begin_date_day,
        end_date_year, end_date_month, end_date_day, ended,
        gender_id, artist_type_id, country_id
      FROM artist
      JOIN artist_revision USING (artist_id)
      JOIN artist_tree USING (artist_tree_id)
      JOIN artist_data USING (artist_data_id)
      JOIN artist_name name ON (artist_data.name = name.id)
      JOIN artist_name sort_name ON (artist_data.sort_name = sort_name.id)
      WHERE artist_id IN ?
        AND revision_id = master_revision_id
    |]


--------------------------------------------------------------------------------
instance ViewRevision Artist where
  viewRevision revision = head <$> query q (Only revision)
    where q = [sql|
       SELECT artist_id, revision_id,
        name.name, sort_name.name, comment,
        begin_date_year, begin_date_month, begin_date_day,
        end_date_year, end_date_month, end_date_day, ended,
        gender_id, artist_type_id, country_id
      FROM artist
      JOIN artist_revision USING (artist_id)
      JOIN artist_tree USING (artist_tree_id)
      JOIN artist_data USING (artist_data_id)
      JOIN artist_name name ON (artist_data.name = name.id)
      JOIN artist_name sort_name ON (artist_data.sort_name = sort_name.id)
      WHERE revision_id = ? |]


--------------------------------------------------------------------------------
instance Create Artist where
  create = Generic.create "artist"


--------------------------------------------------------------------------------
instance Update Artist


--------------------------------------------------------------------------------
instance NewEntityRevision Artist where
  newEntityRevision = Generic.newEntityRevision "artist"


--------------------------------------------------------------------------------
instance RealiseTree Artist where
  realiseTree artist = do
    dataId <- insertArtistData (artistData artist)
    treeId <- insertArtistTree (artistAnnotation artist) dataId

    Generic.realiseRelationships "artist" treeId artist
    Generic.realiseAliases "artist" treeId artist
    Generic.realiseIpiCodes "artist" treeId artist

    return treeId
    where
      insertArtistData :: (Functor m, MonadIO m) => Artist -> MusicBrainzT m Int
      insertArtistData data' = selectValue $
        query [sql| SELECT find_or_insert_artist_data(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) |]
          data'

      insertArtistTree annotationBody dataId = selectValue $
        query [sql| INSERT INTO artist_tree (artist_data_id, annotation)
                    VALUES (?, ?)
                    RETURNING artist_tree_id  |]
          (dataId, annotationBody)


--------------------------------------------------------------------------------
instance ResolveReference Artist where
  resolveReference = Generic.resolveMbid "artist"


--------------------------------------------------------------------------------
instance ResolveReference (Revision Artist) where
  resolveReference = Generic.resolveRevision "artist"


--------------------------------------------------------------------------------
instance CloneRevision Artist where
  cloneRevision = Generic.cloneRevision "artist"


--------------------------------------------------------------------------------
instance Merge Artist
