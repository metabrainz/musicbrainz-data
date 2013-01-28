{-# LANGUAGE ConstraintKinds #-}
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
    , findEditsInvolvingAll
    , viewChanges
    , module MusicBrainz.Edit
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Writer
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid(..), mconcat)
import Data.String (fromString)
import Database.PostgreSQL.Simple (In(..), Only(..), (:.)(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)

import qualified Data.Set as Set

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
  getChanges editId >>= mapM_ mergeUpstream
  closeEdit
  where
    mergeUpstream (ArtistChange r) = mergeRevisionUpstream r
    mergeUpstream (LabelChange r) = mergeRevisionUpstream r
    mergeUpstream (RecordingChange r) = mergeRevisionUpstream r
    mergeUpstream (ReleaseChange r) = mergeRevisionUpstream r
    mergeUpstream (ReleaseGroupChange r) = mergeRevisionUpstream r
    mergeUpstream (UrlChange r) = mergeRevisionUpstream r
    mergeUpstream (WorkChange r) = mergeRevisionUpstream r

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
    linkChange (ArtistChange r) = linkRevisionToEdit editId r
    linkChange (LabelChange r) = linkRevisionToEdit editId r
    linkChange (RecordingChange r) = linkRevisionToEdit editId r
    linkChange (ReleaseChange r) = linkRevisionToEdit editId r
    linkChange (ReleaseGroupChange r) = linkRevisionToEdit editId r
    linkChange (UrlChange r) = linkRevisionToEdit editId r
    linkChange (WorkChange r) = linkRevisionToEdit editId r



--------------------------------------------------------------------------------
mergeRevisionUpstream :: (Applicative m, MonadIO m, FindLatest a, Mergeable (Tree a), NewEntityRevision a, RealiseTree a, MasterRevision a, ViewRevision a, ViewTree a)
  => Ref (Revision a) -> MusicBrainzT m ()
mergeRevisionUpstream new = do
  newVer <- viewRevision new
  let artistId = coreRef newVer

  current <- findLatest artistId
  if coreRevision current == new
    -- We aren't doing a merge at all, but we're simply 'creating' this
    -- entity (by setting an upstream revision).
    then setMasterRevision artistId new

    else do
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


--------------------------------------------------------------------------------
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g) => Monoid (a, b, c, d, e, f, g) where
  mempty = (mempty, mempty, mempty, mempty, mempty, mempty, mempty)
  (a, b, c, d, e, f, g) `mappend` (a', b', c', d', e', f', g') =
    (a `mappend` a', b `mappend` b', c `mappend` c', d `mappend` d', e `mappend` e', f `mappend` f', g `mappend` g')

findEditsInvolvingAll :: (Functor m, MonadIO m) =>
  [CoreEntityRef] -> MusicBrainzT m (Set.Set (Ref Edit))
findEditsInvolvingAll changes =
    Set.fromList . map fromOnly <$> query q (map In $ filter (not . null) $ allRevisions)
  where
    q = fromString $ "SELECT edit_id FROM edit " ++
                     (intercalate " " joins) ++
                     " WHERE " ++
                     (intercalate " AND " preds)
      where

    (artists, labels, recordings, releases, releaseGroups, urls, works) =
        mconcat $ map extractRef changes
      where
        takeRef position c = position .~ [dereference c ^. unwrapped'] $
          ([], [], [], [], [], [], [])

        extractRef (ArtistRef c)       = takeRef _1 c
        extractRef (LabelRef c)        = takeRef _2 c
        extractRef (RecordingRef c)    = takeRef _3 c
        extractRef (ReleaseRef c)      = takeRef _4 c
        extractRef (ReleaseGroupRef c) = takeRef _5 c
        extractRef (UrlRef c)          = takeRef _6 c
        extractRef (WorkRef c)         = takeRef _7 c

    allRevisions = [ artists, labels, recordings, releases, releaseGroups, urls, works ]

    (joins, preds) =
        let joinOn [] _ = ([], [])
            joinOn _  t = ( [ "JOIN edit_" ++ t ++ " USING (edit_id) "
                            , "JOIN " ++ t ++ "_revision " ++ t ++ " ON (" ++ t ++ ".revision_id = edit_" ++ t ++ ".revision_id)" ]
                          , [ "" ++ t ++ "." ++ t ++ "_id IN ?" ]
                          )
        in mconcat [ artists `joinOn` "artist"
                   , labels `joinOn` "label"
                   , recordings `joinOn` "recording"
                   , releaseGroups `joinOn` "release_group"
                   , urls `joinOn` "url"
                   , works `joinOn` "work"
                   ]


--------------------------------------------------------------------------------
viewChanges :: (Applicative m, Functor m, MonadIO m
               , MergeRender (Tree Artist) mo
               , MergeRender (Tree Label) mo
               , MergeRender (Tree Recording) mo
               , MergeRender (Tree Release) mo
               , MergeRender (Tree ReleaseGroup) mo
               , MergeRender (Tree Url) mo
               , MergeRender (Tree Work) mo)
  => Ref Edit -> MusicBrainzT m [[Hunk mo]]
viewChanges e =
    getChanges e >>= mapM view1
  where
    view1 (ArtistChange r) = view1' r
    view1 (LabelChange r) = view1' r
    view1 (RecordingChange r) = view1' r
    view1 (ReleaseChange r) = view1' r
    view1 (ReleaseGroupChange r) = view1' r
    view1 (UrlChange r) = view1' r
    view1 (WorkChange r) = view1' r

    view1' new = do
      newVer <- viewRevision new
      let artistId = coreRef newVer

      current <- findLatest artistId
      ancestor' <- mergeBase new (coreRevision current) >>= traverse viewRevision
      case ancestor' of
        Nothing -> error "Unable to merge: no common ancestor"
        Just ancestor -> do
          newTree <- viewTree new
          currentTree <- viewTree (coreRevision current)
          ancestorTree <- viewTree (coreRevision ancestor)

          return $ fst $ execMerge newTree currentTree ancestorTree merge


--------------------------------------------------------------------------------
getChanges :: (Functor m, MonadIO m) => Ref Edit -> MusicBrainzT m [Change]
getChanges editId = map toChange <$> query [sql|
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
  where
    toChange :: (String, Int) -> Change
    toChange (kind, revisionId) =
      case kind of
        "artist"        -> review change (revisionId ^. reference :: Ref (Revision Artist))
        "label"         -> review change (revisionId ^. reference :: Ref (Revision Label))
        "recording"     -> review change (revisionId ^. reference :: Ref (Revision Recording))
        "release"       -> review change (revisionId ^. reference :: Ref (Revision Release))
        "release_group" -> review change (revisionId ^. reference :: Ref (Revision ReleaseGroup))
        "url"           -> review change (revisionId ^. reference :: Ref (Revision Url))
        "work"          -> review change (revisionId ^. reference :: Ref (Revision Work))
        _               -> error $ "Attempt to load an edit with revision of unknown kind '" ++ kind ++ "'"
