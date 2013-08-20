{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MusicBrainz.Data.Revision.Internal
    ( newUnlinkedRevision
    , addChild
    , newChildRevision
    , CloneRevision(..)
    , runUpdate
    ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Data.Tagged (Tagged, untag)
import Data.String (fromString)

import MusicBrainz
import MusicBrainz.Data.CoreEntity
import MusicBrainz.Edit

--------------------------------------------------------------------------------
{-| Create a new \'system\' revision, that is not yet bound to any entity. -}
newUnlinkedRevision :: (Functor m, MonadIO m)
  => Ref Editor -> MusicBrainzT m (Ref (Revision a))
newUnlinkedRevision editor = selectValue $
  query [sql| INSERT INTO revision (editor_id) VALUES (?) RETURNING revision_id |]
    (Only editor)


--------------------------------------------------------------------------------
{-| Add one 'Revision' as a child of another (parent) 'Revision'. -}
addChild :: (Functor m, MonadIO m) => Ref (Revision a) -> Ref (Revision a) -> MusicBrainzT m ()
addChild childRevision parentRevision =
  void $ execute
    [sql| INSERT INTO revision_parent (revision_id, parent_revision_id)
          VALUES (?, ?) |] (childRevision, parentRevision)


--------------------------------------------------------------------------------
class CloneRevision a where
  cloneRevision :: (Functor m, MonadIO m)
    => CoreEntity a -> Ref Editor -> MusicBrainzT m (Ref (Revision a))

  default cloneRevision
    :: (Functor m, MonadIO m, ToField (Ref a), CoreEntityTable a)
    => CoreEntity a -> Ref Editor -> MusicBrainzT m (Ref (Revision a))
  cloneRevision a editor = do
    revId <- newUnlinkedRevision editor
    selectValue $ query q (coreRef a, revId, coreRevision a)

   where

      entityName = untag (rootTable :: Tagged a String)
      q = fromString $ unlines
          [ "INSERT INTO " ++ entityName ++ "_revision (" ++ entityName ++ "_id, revision_id, " ++ entityName ++ "_tree_id) "
          , "VALUES (?, ?, (SELECT " ++ entityName ++ "_tree_id FROM " ++ entityName ++ "_revision WHERE revision_id = ?)) "
          , "RETURNING revision_id"
          ]



--------------------------------------------------------------------------------
newChildRevision :: (Functor m, MonadIO m, ViewRevision a, NewEntityRevision a)
  => Ref Editor -> Ref (Revision a) -> Ref (Tree a)
  -> MusicBrainzT m (Ref (Revision a))
newChildRevision editorId baseRevisionId treeId = do
  entity <- viewRevision baseRevisionId
  revisionId <- newUnlinkedRevision editorId
  newEntityRevision revisionId (coreRef entity) treeId
  addChild revisionId baseRevisionId
  return revisionId


--------------------------------------------------------------------------------
runUpdate :: Editable a
  => Ref Editor -> Ref (Revision a) -> Tree a -> EditM (Ref (Revision a))
runUpdate editor base tree = do
  treeId <- realiseTree tree
  revisionId <- newChildRevision editor base treeId
  includeRevision revisionId
  return revisionId
