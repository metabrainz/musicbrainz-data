{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MusicBrainz.Data.Revision
    ( newRevision ) where

import Database.PostgreSQL.Simple (Only(Only))
import Database.PostgreSQL.Simple.SqlQQ

import MusicBrainz

newRevision :: Ref Editor -> MusicBrainz (Ref (Revision a))
newRevision editor = selectValue $
  query [sql| INSERT INTO revision (editor_id) VALUES (?) RETURNING revision_id |]
    (Only editor)
