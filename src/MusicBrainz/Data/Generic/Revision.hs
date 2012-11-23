{-# LANGUAGE FlexibleContexts #-}
module MusicBrainz.Data.Generic.Revision where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.String (fromString)
import Database.PostgreSQL.Simple.ToField (ToField)

import MusicBrainz
import MusicBrainz.Data.Revision.Internal

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
