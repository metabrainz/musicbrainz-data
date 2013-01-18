{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Data.Generic
    ( viewAliases
    , viewAnnotation
    , viewIpiCodes
    , MusicBrainz.Data.Generic.cloneRevision
    , create
    , MusicBrainz.Data.Generic.linkRevisionToEdit
    , MusicBrainz.Data.Generic.newEntityRevision
    , resolveMbid
    , resolveRevision
    , MusicBrainz.Data.Generic.setMasterRevision
    , realiseAliases
    , realiseIpiCodes
    , realiseRelationships
    , fetchEndPoints
    , reflectRelationshipChange
    , addRelationship
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Foldable (forM_)
import Database.PostgreSQL.Simple (Only(..), (:.)(..))
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField (ToField)

import qualified Data.Set as Set

import MusicBrainz
import MusicBrainz.Lens
import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.Revision.Internal
import MusicBrainz.Data.Tree
import MusicBrainz.Edit
import MusicBrainz.Types.Internal

import {-# SOURCE #-} MusicBrainz.Data.Artist ()
import {-# SOURCE #-} MusicBrainz.Data.Label ()
import {-# SOURCE #-} MusicBrainz.Data.Recording ()
import {-# SOURCE #-} MusicBrainz.Data.Release ()
import {-# SOURCE #-} MusicBrainz.Data.ReleaseGroup ()
import {-# SOURCE #-} MusicBrainz.Data.Url ()
import {-# SOURCE #-} MusicBrainz.Data.Work ()

--------------------------------------------------------------------------------
viewAliases :: (Functor m, MonadIO m)
                 => String -> Ref (Revision a) -> MusicBrainzT m (Set.Set Alias)
viewAliases entityName r = Set.fromList <$> query q (Only r)
  where
    q = fromString $ unlines
          [ "SELECT name.name, sort_name.name,"
          , "begin_date_year, begin_date_month, begin_date_day,"
          , "end_date_year, end_date_month, end_date_day,"
          , "ended, " ++ entityName ++ "_alias_type_id, locale, primary_for_locale "
          , "FROM " ++ entityName ++ "_alias alias"
          , "JOIN " ++ entityName ++ "_name name ON (alias.name = name.id) "
          , "JOIN " ++ entityName ++ "_name sort_name ON (alias.sort_name = sort_name.id) "
          , "JOIN " ++ entityName ++ "_tree USING (" ++ entityName ++ "_tree_id) "
          , "JOIN " ++ entityName ++ "_revision USING (" ++ entityName ++ "_tree_id) "
          , "WHERE revision_id = ?"
          ]


--------------------------------------------------------------------------------
viewAnnotation :: (Functor m, MonadIO m)
                 => String -> Ref (Revision a) -> MusicBrainzT m Text
viewAnnotation entityName r = fromOnly . head <$> query q (Only r)
  where
    q = fromString $ unlines
        [ "SELECT annotation "
        , "FROM " ++ entityName ++ "_tree "
        , "JOIN " ++ entityName ++ "_revision USING (" ++ entityName ++ "_tree_id) "
        , "WHERE revision_id = ?"
        ]


--------------------------------------------------------------------------------
create :: (Editable a, FromField (Ref a), MasterRevision a, NewEntityRevision a, RealiseTree a)
  => String -> Ref Editor -> Tree a -> EditM (Ref (Revision a))
create eName editor entity = do
  treeId <- realiseTree entity
  entityId <- reserveEntityTable eName
  revisionId <- newUnlinkedRevision editor
  MusicBrainz.Edit.newEntityRevision revisionId entityId treeId
  MusicBrainz.Edit.setMasterRevision entityId revisionId
  includeRevision revisionId
  return revisionId


--------------------------------------------------------------------------------
reserveEntityTable :: (Functor m, MonadIO m, FromField (Ref a)) => String -> MusicBrainzT m (Ref a)
reserveEntityTable table = selectValue $ query_ $
  fromString ("INSERT INTO " ++ table ++ " (master_revision_id) VALUES (-1) RETURNING " ++ table  ++ "_id")


--------------------------------------------------------------------------------
linkRevisionToEdit :: (Functor m, MonadIO m)
  => String -> Ref Edit -> Ref (Revision a) -> MusicBrainzT m ()
linkRevisionToEdit table editId revisionId = void $
  execute q (editId, revisionId)
  where q = fromString $ unlines
              [ "INSERT INTO " ++ table ++ " (edit_id, revision_id)"
              , " VALUES (?, ?)"
              ]


--------------------------------------------------------------------------------
viewIpiCodes :: (Functor m, MonadIO m)
                 => String -> Ref (Revision a) -> MusicBrainzT m (Set.Set IPI)
viewIpiCodes entityName r = Set.fromList <$> query q (Only r)
  where q = fromString $ unlines
            [ "SELECT ipi "
            , "FROM " ++ entityName ++ "_ipi "
            , "JOIN " ++ entityName ++ "_tree USING (" ++ entityName ++ "_tree_id) "
            , "JOIN " ++ entityName ++ "_revision USING (" ++ entityName ++ "_tree_id) "
            , "WHERE revision_id = ?"
            ]


--------------------------------------------------------------------------------
resolveMbid :: (Functor m, MonadIO m, FromField (Ref a))
  => String -> MBID a -> MusicBrainzT m (Maybe (Ref a))
resolveMbid eName entityMbid =
  listToMaybe . map fromOnly <$> query q (Only entityMbid)
  where
    q = fromString $ unlines
        [ "WITH RECURSIVE path (revision_id, " ++ eName ++ "_id, child_revision_id, created_at, is_master_revision_id) "
        , "AS ("
        , "  SELECT "
        , "    " ++ eName ++ "_revision.revision_id, "
        , "    " ++ eName ++ "_revision." ++ eName ++ "_id, "
        , "    revision_parent.revision_id AS child_revision_id, "
        , "    created_at, "
        , "    TRUE as is_master_revision_id "
        , "  FROM " ++ eName ++ "_revision "
        , "  JOIN " ++ eName ++ " USING (" ++ eName ++ "_id) "
        , "  JOIN revision USING (revision_id) "
        , "  LEFT JOIN revision_parent ON (revision_parent.parent_revision_id = revision.revision_id) "
        , "  WHERE " ++ eName ++ "_id = ? AND master_revision_id = " ++ eName ++ "_revision.revision_id "
        , " "
        , "  UNION "
        , " "
        , "  SELECT "
        , "    " ++ eName ++ "_revision.revision_id, "
        , "    " ++ eName ++ "_revision." ++ eName ++ "_id, "
        , "    revision_parent.revision_id, "
        , "    revision.created_at, "
        , "    master_revision_id = " ++ eName ++ "_revision.revision_id AS is_master_revision_id "
        , "  FROM path "
        , "  JOIN " ++ eName ++ "_revision ON (path.child_revision_id = " ++ eName ++ "_revision.revision_id) "
        , "  JOIN revision ON (revision.revision_id = " ++ eName ++ "_revision.revision_id) "
        , "  JOIN " ++ eName ++ " ON (" ++ eName ++ "." ++ eName ++ "_id = " ++ eName ++ "_revision." ++ eName ++ "_id) "
        , "  LEFT JOIN revision_parent ON (revision_parent.parent_revision_id = " ++ eName ++ "_revision.revision_id) "
        , ") "
        , "SELECT " ++ eName ++ "_id "
        , "FROM path "
        , "WHERE is_master_revision_id "
        , "ORDER BY created_at, revision_id DESC "
        , "LIMIT 1 "
        ]


--------------------------------------------------------------------------------
resolveRevision :: (Functor m, MonadIO m, FromField (Ref a))
  => String -> RefSpec (Revision a) -> MusicBrainzT m (Maybe (Ref a))
resolveRevision eName revisionId =
  listToMaybe . map fromOnly <$> query q (Only revisionId)
  where
    q = fromString $ unlines
        [ "SELECT revision_id "
        , "FROM " ++ eName ++ "_revision "
        , "WHERE revision_id = ?"
        ]


--------------------------------------------------------------------------------
setMasterRevision :: (Functor m, MonadIO m, ToField (Ref a))
  => String -> Ref a -> Ref (Revision a) -> MusicBrainzT m ()
setMasterRevision table entityId revisionId = void $
  execute q (revisionId, entityId)
  where
    q = fromString $ unlines
      [ "UPDATE " ++ table ++ " SET master_revision_id = ? "
      , "WHERE " ++ table ++ "_id = ?"
      ]


--------------------------------------------------------------------------------
cloneRevision :: (Functor m, MonadIO m, ToField (Ref a))
  => String -> CoreEntity a -> Ref Editor -> MusicBrainzT m (Ref (Revision a))
cloneRevision entityName a editor = do
  revId <- newUnlinkedRevision editor
  selectValue $ query q (coreRef a, revId, coreRevision a)
  where
    q = fromString $ unlines
        [ "INSERT INTO " ++ entityName ++ "_revision (" ++ entityName ++ "_id, revision_id, " ++ entityName ++ "_tree_id) "
        , "VALUES (?, ?, (SELECT " ++ entityName ++ "_tree_id FROM " ++ entityName ++ "_revision WHERE revision_id = ?)) "
        , "RETURNING revision_id"
        ]


--------------------------------------------------------------------------------
newEntityRevision :: (Functor m, MonadIO m, ToField (Ref a))
  => String -> Ref (Revision a) -> Ref a -> Ref (Tree a) -> MusicBrainzT m ()
newEntityRevision entityName revisionId entityId entityTreeId = void $
  execute q (entityId, revisionId, entityTreeId)
  where
    q = fromString $ unlines
        [ "INSERT INTO " ++ entityName ++ "_revision (" ++ entityName ++ "_id, revision_id, " ++ entityName ++ "_tree_id) "
        , "VALUES (?, ?, ?)"
        ]


--------------------------------------------------------------------------------
realiseAliases :: (Functor m, MonadIO m, TreeAliases a) => String -> Ref (Tree a) -> Tree a -> MusicBrainzT m ()
realiseAliases eName treeId tree = forM_ (Set.toList $ tree^.aliases) $ \alias ->
  execute q (Only treeId :. alias)
  where q = fromString $ unlines
          [ "INSERT INTO " ++ eName ++ "_alias (" ++ eName ++ "_tree_id, name, sort_name, "
          , "begin_date_year, begin_date_month, begin_date_day, "
          , "end_date_year, end_date_month, end_date_day, "
          , "ended, " ++ eName ++ "_alias_type_id, locale, primary_for_locale) "
          , "VALUES (?, (SELECT find_or_insert_" ++ eName ++ "_name(?)), "
          , "(SELECT find_or_insert_" ++ eName ++ "_name(?)), ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
          ]


--------------------------------------------------------------------------------
realiseIpiCodes :: (Functor m, MonadIO m, TreeIPICodes a)
  => String -> Ref (Tree a) -> Tree a -> MusicBrainzT m ()
realiseIpiCodes eName treeId tree = void $ executeMany q
      $ map (Only treeId :.) (Set.toList $ tree^.ipiCodes)
  where q = fromString $ "INSERT INTO " ++ eName ++ "_ipi (" ++ eName ++ "_tree_id, ipi) VALUES (?, ?)"


--------------------------------------------------------------------------------
fetchEndPoints :: (Functor m, MonadIO m)
  => String -> Ref (Revision a) -> RelationshipTarget
  -> MusicBrainzT m [(Relationship -> LinkedRelationship, Int)]
fetchEndPoints source r t = case t of
    ToArtist -> fetch ArtistRelationship "artist"
    ToLabel -> fetch LabelRelationship "label"
    ToRecording -> fetch RecordingRelationship "recording"
    ToRelease -> fetch ReleaseRelationship "release"
    ToReleaseGroup -> fetch ReleaseGroupRelationship "release_group"
    ToUrl -> fetch UrlRelationship "url"
    ToWork -> fetch WorkRelationship "work"
  where
    fetch cons t1 =
      let q = fromString $ unlines
            [ "SELECT l." ++ t1 ++ "_id, l.relationship_id "
            , "FROM l_" ++ source ++ "_" ++ t1 ++ " l "
            , "JOIN " ++ source ++ "_tree source_tree ON (l." ++ source ++ "_tree_id = source_tree." ++ source ++ "_tree_id) "
            , "JOIN " ++ source ++ "_revision source ON (source." ++ source ++ "_tree_id = source_tree." ++ source ++ "_tree_id) "
            , "WHERE source.revision_id = ?"
            ]
      in map (constructPartialRel cons) <$> query q (Only r)
    constructPartialRel cons (targetId, relationshipId) =
      (cons targetId, relationshipId)


--------------------------------------------------------------------------------
reflectRelationshipChange :: (Ref a -> Relationship -> LinkedRelationship)
                          -> Ref Editor
                          -> Ref a
                          -> (LinkedRelationship -> Set.Set LinkedRelationship -> Set.Set LinkedRelationship)
                          -> LinkedRelationship
                          -> EditM ()
reflectRelationshipChange returnCon editor endpoint f toReflect =
  case toReflect of
    (ArtistRelationship targetId rel) -> reflect targetId rel
    (LabelRelationship targetId rel) -> reflect targetId rel
    (RecordingRelationship targetId rel) -> reflect targetId rel
    (ReleaseRelationship targetId rel) -> reflect targetId rel
    (ReleaseGroupRelationship targetId rel) -> reflect targetId rel
    (UrlRelationship targetId rel) -> reflect targetId rel
    (WorkRelationship targetId rel) -> reflect targetId rel
  where
    reflect targetId rel = do
      let returnRelationship = returnCon endpoint rel
      target <- findLatest targetId
      targetTree <- over relationships (f returnRelationship) <$> viewTree (coreRevision target)
      void $ runUpdate editor (coreRevision target) targetTree


--------------------------------------------------------------------------------
addRelationship :: (Functor m, MonadIO m)
  => String -> Ref (Tree a) -> LinkedRelationship -> MusicBrainzT m ()
addRelationship source treeId rel = case rel of
    (ArtistRelationship targetId relInfo) -> go "artist" targetId relInfo
    (LabelRelationship targetId relInfo) -> go "label" targetId relInfo
    (RecordingRelationship targetId relInfo) -> go "recording" targetId relInfo
    (ReleaseRelationship targetId relInfo) -> go "release" targetId relInfo
    (ReleaseGroupRelationship targetId relInfo) -> go "release_group" targetId relInfo
    (WorkRelationship targetId relInfo) -> go "work" targetId relInfo
    (UrlRelationship targetId relInfo) -> go "url" targetId relInfo
  where
    go target targetId relInfo = do
      relationshipId <- selectValue $ query [sql|
        INSERT INTO relationship (relationship_type_id,
          begin_date_year, begin_date_month, begin_date_day,
          end_date_year, end_date_month, end_date_day,
          ended)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?) RETURNING relationship_id |] relInfo
      let q = fromString $ unlines
            [ "INSERT INTO l_" ++ source ++ "_" ++ target ++ " "
            , "(" ++ source ++ "_tree_id, " ++ target ++ "_id, relationship_id) "
            , "VALUES (?, ?, ?)"
            ]
      void $ execute q (treeId, targetId, relationshipId :: Int)


--------------------------------------------------------------------------------
realiseRelationships :: (Functor m, MonadIO m, TreeRelationships a)
  => String -> Ref (Tree a) -> Tree a -> MusicBrainzT m ()
realiseRelationships tbl treeId =
  mapMOf_ (relationships.folded) (addRelationship tbl treeId)
