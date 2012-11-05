{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MusicBrainz.Data.Revision
    ( newRevision, addChild ) where

import Control.Monad (void)
import Database.PostgreSQL.Simple (Only(Only))
import Database.PostgreSQL.Simple.SqlQQ

import MusicBrainz

--------------------------------------------------------------------------------
newRevision :: Ref Editor -> MusicBrainz (Ref (Revision a))
newRevision editor = selectValue $
  query [sql| INSERT INTO revision (editor_id) VALUES (?) RETURNING revision_id |]
    (Only editor)

--------------------------------------------------------------------------------
addChild :: Ref (Revision a) -> Ref (Revision a) -> MusicBrainz ()
addChild childRevision parentRevision =
  void $ execute
    [sql| INSERT INTO revision_parent (revision_id, parent_revision_id)
          VALUES (?, ?) |] (childRevision, parentRevision)
