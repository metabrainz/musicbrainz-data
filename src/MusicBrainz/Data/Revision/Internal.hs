{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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

import MusicBrainz
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
