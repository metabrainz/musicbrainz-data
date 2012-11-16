{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions to work with MusicBrainz edits. -}
module MusicBrainz.Data.Edit
    ( apply
    , openEdit
    , createEdit
    , addEditNote
    , findEditNotes
    , voteOnEdit
    , module MusicBrainz.Edit
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Writer
import Database.PostgreSQL.Simple (Only(..), (:.)(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz
import MusicBrainz.Data.Artist ()
import MusicBrainz.Edit
import MusicBrainz.Types.Internal

--------------------------------------------------------------------------------
{-| Apply an edit by merging all revisions in the edit upstream. -}
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
{-| Open a fresh edit, which can then have revisions added. -}
openEdit :: MusicBrainz (Ref Edit)
openEdit = selectValue $ query
  [sql| INSERT INTO edit (status) VALUES (?) RETURNING edit_id |]
    (Only Open)


--------------------------------------------------------------------------------
{-| Append an edit note to the list of edit notes for an edit. -}
addEditNote :: Ref Edit -> EditNote -> MusicBrainz ()
addEditNote editId note = void $ execute
  [sql| INSERT INTO edit_note (edit_id, editor_id, text) VALUES (?, ?, ?) |]
    (Only editId :. note)


--------------------------------------------------------------------------------
{-| Find all edit notes for an edit. -}
findEditNotes :: Ref Edit -> MusicBrainz [Entity EditNote]
findEditNotes editId = query
  [sql| SELECT edit_note_id, text, editor_id FROM edit_note WHERE edit_id = ? |]
    (Only editId)


--------------------------------------------------------------------------------
{-| Allow an editor to cast a 'Vote' on an edit. -}
voteOnEdit :: Ref Editor -> Ref Edit -> Vote -> MusicBrainz ()
voteOnEdit editorId editId vote = void $ execute
  [sql| INSERT INTO vote (edit_id, editor_id, vote) VALUES (?, ?, ?) |]
    (editorId, editId, vote)


--------------------------------------------------------------------------------
{-| Create an edit, and run an 'EditM' action to create the various components
of the edit (that is, link revisions to the edit). -}
createEdit :: EditM a -> MusicBrainz (Ref Edit)
createEdit actions = do
  editId <- openEdit
  changes <- fmap execWriterT (nestMb actions) >>= liftIO
  mapM_ (linkChange editId) changes
  return editId
  where linkChange editId (Change r) = linkRevisionToEdit editId r
