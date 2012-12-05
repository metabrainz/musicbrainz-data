{-# LANGUAGE FlexibleContexts #-}
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
    , MusicBrainz.Data.Generic.setMasterRevision
    , realiseAliases
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Foldable (forM_)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple (Only(..), (:.)(..))
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)

import qualified Data.Set as Set

import MusicBrainz
import MusicBrainz.Lens
import MusicBrainz.Data.Revision.Internal
import MusicBrainz.Edit

--------------------------------------------------------------------------------
viewAliases :: (Functor m, MonadIO m)
                 => String -> Ref (Revision a) -> MusicBrainzT m (Set.Set Alias)
viewAliases entityName r = Set.fromList <$> query q (Only r)
  where
    q = fromString $ unlines
          [ "SELECT name.name, sort_name.name,"
          , "begin_date_year, begin_date_month, begin_date_day,"
          , "end_date_year, end_date_month, end_date_day,"
          , "ended, " ++ entityName ++ "_alias_type_id, locale "
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
create :: (Editable a, FromField (Ref a), MasterRevision a, NewEntityRevision a, RealiseTree a, Typeable a)
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
reserveEntityTable :: (Functor m, MonadIO m, Typeable a, FromField (Ref a)) => String -> MusicBrainzT m (Ref a)
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
realiseAliases eName treeId tree = forM_ (Set.toList $ tree^.aliases) $ \alias -> do
  execute q (Only treeId :. alias)
  where q = fromString $ unlines
          [ "INSERT INTO " ++ eName ++ "_alias (" ++ eName ++ "_tree_id, name, sort_name, "
          , "begin_date_year, begin_date_month, begin_date_day, "
          , "end_date_year, end_date_month, end_date_day, "
          , "ended, " ++ eName ++ "_alias_type_id, locale) "
          , "VALUES (?, (SELECT find_or_insert_" ++ eName ++ "_name(?)), "
          , "(SELECT find_or_insert_" ++ eName ++ "_name(?)), ?, ?, ?, ?, ?, ?, ?, ?, ?)"
          ]
