{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MusicBrainz.Data.Edit
    ( apply
    , openEdit
    , module MusicBrainz.Edit
    ) where

import Control.Applicative
import Control.Monad
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz
import MusicBrainz.Data.Artist
import MusicBrainz.Edit

--------------------------------------------------------------------------------
apply :: Ref Edit -> MusicBrainz ()
apply editId = getChanges >>= mapM_ merge
  where
    getChanges = map toChange <$> query [sql|
      SELECT 'artist'::text, revision_id FROM edit_artist WHERE edit_id = ?
    |] (Only editId)
    merge (Change r) = mergeRevisionUpstream r

    toChange :: (String, Int) -> Change
    toChange (kind, revisionId) =
      case kind of
        "artist" -> Change (RevisionRef revisionId :: Ref (Revision Artist))
        _ -> error $ "Attempt to load an edit with revision of unknown kind '" ++ kind ++ "'"


--------------------------------------------------------------------------------
openEdit :: MusicBrainz (Ref Edit)
openEdit = selectValue $ query_
  [sql| INSERT INTO edit DEFAULT VALUES RETURNING edit_id |]
