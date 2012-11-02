{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MusicBrainz.Data.Edit
    ( apply
    , openEdit
    , addEditNote
    , findEditNotes
    , module MusicBrainz.Edit
    ) where

import Control.Applicative
import Control.Monad
import Database.PostgreSQL.Simple (Only(..), (:.)(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz
import MusicBrainz.Data.Artist
import MusicBrainz.Edit

--------------------------------------------------------------------------------
apply :: Ref Edit -> MusicBrainz ()
apply editId = do
  getChanges >>= mapM_ merge
  closeEdit
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

    closeEdit = void $ execute
      [sql| UPDATE edit SET status = ? WHERE edit_id = ? |] (Closed, editId)


--------------------------------------------------------------------------------
openEdit :: MusicBrainz (Ref Edit)
openEdit = selectValue $ query_
  [sql| INSERT INTO edit ( status ) DEFAULT VALUES RETURNING edit_id |]


--------------------------------------------------------------------------------
addEditNote :: Ref Edit -> EditNote -> MusicBrainz ()
addEditNote editId note = void $ execute
  [sql| INSERT INTO edit_note (edit_id, editor_id, text) VALUES (?, ?, ?) |]
    (Only editId :. note)


--------------------------------------------------------------------------------
findEditNotes :: Ref Edit -> MusicBrainz [Entity EditNote]
findEditNotes editId = query
  [sql| SELECT editor_id, text FROM edit_note WHERE edit_id = ? |]
    (Only editId)


--------------------------------------------------------------------------------
voteOnEdit :: Ref Editor -> Ref Edit -> Vote -> MusicBrainz ()
voteOnEdit editorId editId vote = void $ execute
  [sql| INSERT INTO vote (edit_id, editor_id, vote) VALUES (?, ?, ?) |]
    (editorId, editId, vote)
