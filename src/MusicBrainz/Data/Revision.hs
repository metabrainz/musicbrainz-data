{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MusicBrainz.Data.Revision
    ( newRevision, addChild ) where

import Control.Monad (void)
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple (Only(Only))
import Database.PostgreSQL.Simple.SqlQQ

import MusicBrainz

--------------------------------------------------------------------------------
newRevision :: (Functor m, MonadIO m) => Ref Editor -> MusicBrainzT m (Ref (Revision a))
newRevision editor = selectValue $
  query [sql| INSERT INTO revision (editor_id) VALUES (?) RETURNING revision_id |]
    (Only editor)

--------------------------------------------------------------------------------
addChild :: (Functor m, MonadIO m) => Ref (Revision a) -> Ref (Revision a) -> MusicBrainzT m ()
addChild childRevision parentRevision =
  void $ execute
    [sql| INSERT INTO revision_parent (revision_id, parent_revision_id)
          VALUES (?, ?) |] (childRevision, parentRevision)
