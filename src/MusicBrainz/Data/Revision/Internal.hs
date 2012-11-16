{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MusicBrainz.Data.Revision.Internal
    ( newRevision
    , addChild
    , CloneRevision(..)
    ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz

--------------------------------------------------------------------------------
{-| Create a new \'system\' revision, that is not yet bound to any entity. -}
newRevision :: (Functor m, MonadIO m) => Ref Editor -> MusicBrainzT m (Ref (Revision a))
newRevision editor = selectValue $
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
