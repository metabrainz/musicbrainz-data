{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-| Functions to work with MusicBrainz edits. -}
module MusicBrainz.Data.Edit
    ( apply
    , openEdit
    , createEdit
    , withEdit
    , addEditNote
    , findEditNotes
    , voteOnEdit
    , listVotes
    , module MusicBrainz.Edit
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Writer
import Data.Maybe (listToMaybe)
import Data.Traversable (traverse)
import Database.PostgreSQL.Simple (Only(..), (:.)(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import MusicBrainz
import MusicBrainz.Data.Artist ()
import MusicBrainz.Data.FindLatest (findLatest, FindLatest, ResolveReference(..))
import MusicBrainz.Data.Label ()
import MusicBrainz.Data.Recording ()
import MusicBrainz.Data.Release ()
import MusicBrainz.Data.ReleaseGroup ()
import MusicBrainz.Data.Revision (mergeBase)
import MusicBrainz.Data.Revision.Internal (addChild, newChildRevision)
import MusicBrainz.Data.Tree (viewTree, ViewTree)
import MusicBrainz.Data.Url ()
import MusicBrainz.Data.Work ()
import MusicBrainz.Edit
import MusicBrainz.Merge
import MusicBrainz.Types.Internal

--------------------------------------------------------------------------------
{-| Apply an edit by merging all revisions in the edit upstream. -}
apply :: Ref Edit -> MusicBrainz ()
apply editId = do
  getChanges >>= mapM_ mergeUpstream
  closeEdit
  where
    getChanges = map toChange <$> query [sql|
      SELECT 'artist'::text, revision_id FROM edit_artist WHERE edit_id = ?
      UNION ALL
      SELECT 'label'::text, revision_id FROM edit_label WHERE edit_id = ?
      UNION ALL
      SELECT 'recording'::text, revision_id FROM edit_recording WHERE edit_id = ?
      UNION ALL
      SELECT 'release'::text, revision_id FROM edit_release WHERE edit_id = ?
      UNION ALL
      SELECT 'release_group'::text, revision_id FROM edit_release_group WHERE edit_id = ?
      UNION ALL
      SELECT 'url'::text, revision_id FROM edit_url WHERE edit_id = ?
      UNION ALL
      SELECT 'work'::text, revision_id FROM edit_work WHERE edit_id = ?
    |] (editId, editId, editId, editId, editId, editId, editId)
    mergeUpstream (Change r) = mergeRevisionUpstream r

    toChange :: (String, Int) -> Change
    toChange (kind, revisionId) =
      case kind of
        "artist" -> Change (Ref revisionId :: Ref (Revision Artist))
        "label" -> Change (Ref revisionId :: Ref (Revision Label))
        "recording" -> Change (Ref revisionId :: Ref (Revision Recording))
        "release" -> Change (Ref revisionId :: Ref (Revision Release))
        "release_group" -> Change (Ref revisionId :: Ref (Revision ReleaseGroup))
        "url" -> Change (Ref revisionId :: Ref (Revision Url))
        "work" -> Change (Ref revisionId :: Ref (Revision Work))
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
voteOnEdit :: Ref Editor -> Ref Edit -> VoteScore -> MusicBrainz ()
voteOnEdit editorId editId vote = void $ execute
  [sql| INSERT INTO vote (edit_id, editor_id, vote) VALUES (?, ?, ?) |]
    (editId, editorId, vote)


--------------------------------------------------------------------------------
{-| Create an edit, and run an 'EditM' action to create the various components
of the edit (that is, link revisions to the edit). -}
createEdit :: EditM a -> MusicBrainz (Ref Edit)
createEdit actions = do
  editId <- openEdit
  runEditM editId actions
  return editId


--------------------------------------------------------------------------------
{-| Given an edit that already exists, run 'EditM' actions against it, to
augment the edit with additional changes. -}
withEdit :: Ref Edit -> EditM a -> MusicBrainz a
withEdit editId action = fst <$> runEditM editId action


--------------------------------------------------------------------------------
runEditM :: Ref Edit -> EditM a -> MusicBrainz (a, [Change])
runEditM editId action = do
  (a, changes) <- fmap runWriterT (nestMb action) >>= liftIO
  mapM_ linkChange changes
  return (a, changes)
  where
    linkChange (Change r) = linkRevisionToEdit editId r


--------------------------------------------------------------------------------
mergeRevisionUpstream :: (Applicative m, MonadIO m, FindLatest a, Mergeable (Tree a), NewEntityRevision a, RealiseTree a, MasterRevision a, ViewRevision a, ViewTree a)
  => Ref (Revision a) -> MusicBrainzT m ()
mergeRevisionUpstream new = do
  newVer <- viewRevision new
  let artistId = coreRef newVer

  current <- findLatest artistId
  case coreRevision current == new of
    -- We aren't doing a merge at all, but we're simply 'creating' this
    -- entity (by setting an upstream revision).
    True -> do
      setMasterRevision artistId new

    False -> do
      ancestor' <- mergeBase new (coreRevision current) >>= traverse viewRevision
      case ancestor' of
        Nothing -> error "Unable to merge: no common ancestor"
        Just ancestor -> do
          newTree <- viewTree new
          currentTree <- viewTree (coreRevision current)
          ancestorTree <- viewTree (coreRevision ancestor)

          case runMerge newTree currentTree ancestorTree merge of
            Nothing -> error "Unable to merge: conflict"
            Just merged -> do
              editorId <- selectValue $ query
                [sql| SELECT editor_id FROM revision WHERE revision_id = ? |]
                  (Only $ coreRevision current)

              treeId <- realiseTree merged
              revisionId <- newChildRevision editorId (coreRevision current) treeId
              addChild revisionId new

              setMasterRevision artistId revisionId


--------------------------------------------------------------------------------
listVotes :: (Functor m, Monad m, MonadIO m) => Ref Edit -> MusicBrainzT m [Vote]
listVotes editId =
  query [sql|
      SELECT vote, editor_id,
        row_number() OVER (PARTITION BY editor_id ORDER BY vote_time DESC) > 1 AS superceded
      FROM vote
      WHERE edit_id = ?
      ORDER BY vote_time ASC
    |] (Only editId)


--------------------------------------------------------------------------------
instance ResolveReference Edit where
  resolveReference editId = listToMaybe . map fromOnly <$> query q (Only editId)
    where q = [sql| SELECT edit_id FROM edit WHERE edit_id = ? |]
