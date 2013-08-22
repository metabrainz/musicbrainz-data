{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MusicBrainz.Edit where

import Control.Applicative
import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer (WriterT(..), tell)
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Data.Tagged (Tagged, untag)
import Data.Text (Text)
import Database.PostgreSQL.Simple ((:.)(..), Only(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.ToRow (ToRow(..))
import GHC.Enum (boundedEnumFrom)

import MusicBrainz.Util (viewOnce)
import MusicBrainz.Monad
import MusicBrainz.Merge
import MusicBrainz.Class.FindLatest
import MusicBrainz.Class.MasterRevision
import MusicBrainz.Class.NewEntityRevision
import MusicBrainz.Class.RealiseTree
import MusicBrainz.Class.ResolveReference
import MusicBrainz.Class.RootTable
import MusicBrainz.Class.ViewRevision
import MusicBrainz.Editor (Editor)
import MusicBrainz.Entity
import MusicBrainz.Ref (Ref, Referenceable(..), reference, dereference)
import MusicBrainz.Revision (Revision, mergeBase)
import MusicBrainz.Revision.Internal
import MusicBrainz.Tree (Tree, ViewTree(..))


--------------------------------------------------------------------------------
{-| An edit bundles up multiple 'Revision's that have not yet been applied to
entities. Editors can then vote on these edits to decide if they should be
merge, which ModBot can then later merge (or reject) once a consensus
emerges. -}
data Edit = Edit
    { editStatus :: EditStatus
    }
  deriving (Eq, Show)

instance Referenceable Edit where
  type RefSpec Edit = Int

instance FromField (Ref Edit) where
  fromField f v = view reference <$> fromField f v

instance ToField EditStatus where
  toField = toField . fromEnum

instance ToField (Ref Edit) where
  toField = toField . dereference

instance ResolveReference Edit where
  resolveReference editId = listToMaybe . map fromOnly <$> query q (Only editId)
    where q = [sql| SELECT edit_id FROM edit WHERE edit_id = ? |]


--------------------------------------------------------------------------------
{-| The possible states an edit can be in. -}
data EditStatus = Open | Closed
  deriving (Eq, Show)

instance Enum EditStatus where
  fromEnum Open = 1
  fromEnum Closed = 2

  toEnum 1 = Open
  toEnum 2 = Closed
  toEnum n = error $ show n ++ " cannot be converted to EditStatus"

  enumFrom = boundedEnumFrom

instance Bounded EditStatus where
  minBound = Open
  maxBound = Closed


--------------------------------------------------------------------------------
{-| An edit note is a comment that can be left by editors on edit notes, to
have a discussion about the changes being made, or to provide references for
other editors to verify changes against. -}
data EditNote = EditNote
    { editNoteBody :: !Text
    , editNoteAuthor :: !(Ref Editor)
    }
  deriving (Eq, Show)

instance Referenceable EditNote where
  type RefSpec EditNote = Int

instance FromField (Ref EditNote) where
  fromField f v = view reference <$> fromField f v

instance FromRow EditNote where
  fromRow = EditNote <$> field <*> field

instance ToRow EditNote where
  toRow EditNote{..} = [ toField editNoteAuthor
                       , toField editNoteBody
                       ]


--------------------------------------------------------------------------------
{-| Given an edit that already exists, run 'EditM' actions against it, to
augment the edit with additional changes. -}
withEdit :: Ref Edit -> EditT a -> MusicBrainz a
withEdit editId action = fst <$> runEditT editId action


--------------------------------------------------------------------------------
{-| Open a fresh edit, which can then have revisions added. -}
openEdit :: MusicBrainz (Ref Edit)
openEdit = selectValue $ query
  [sql| INSERT INTO edit (status) VALUES (?) RETURNING edit_id |]
    (Only Open)


--------------------------------------------------------------------------------
mergeRevisionUpstream
  :: (Applicative m, MonadIO m, FindLatest a, Mergeable (Tree a)
     , NewEntityRevision a, RealiseTree a, MasterRevision a, ViewRevision a
     , ViewTree a)
  => Ref (Revision a) -> MusicBrainzT m ()
mergeRevisionUpstream new = do
  newVer <- viewRevision new
  let artistId = coreRef newVer

  current <- viewOnce findLatest artistId
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

          case runMerge newTree currentTree ancestorTree MusicBrainz.Merge.merge of
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
data Change = forall entity.
  Editable entity => Change (Ref (Revision entity))


--------------------------------------------------------------------------------
{-| Accumulate many changes inside a single Edit. -}
type EditT = MusicBrainzT (WriterT [Change] IO)


--------------------------------------------------------------------------------
{-| Include a specific 'Revision' as part of an edit.

This is a fairly low-level operation, and you should be careful that you only
include revisions that haven't already been merged! -}
includeRevision :: Editable a => Ref (Revision a) -> EditT ()
includeRevision = lift . tell . return . Change


--------------------------------------------------------------------------------
{-| The 'Editable' class has instances which have versioning and thus can be
included in edits. -}
--class (FindLatest a, MasterRevision a, Mergeable (Tree a),
class (FindLatest a, Mergeable (Tree a), MasterRevision a, NewEntityRevision a, RealiseTree a, ViewTree a, ViewRevision a) => Editable a where
  {-| Add a revision into an edit. -}
  linkRevisionToEdit :: Ref Edit -> Ref (Revision a) -> MusicBrainz ()

  default linkRevisionToEdit
    :: (RootTable a, ToField (Ref Edit), ToField (Ref (Revision a)))
    => Ref Edit -> Ref (Revision a) -> MusicBrainz ()
  linkRevisionToEdit editId revisionId = void $ execute q (editId, revisionId)
   where
    table = "edit_" ++ untag (rootTable :: Tagged a String)
    q = fromString $ unlines
          [ "INSERT INTO " ++ table ++ " (edit_id, revision_id)"
          , " VALUES (?, ?)"
          ]


--------------------------------------------------------------------------------
runEditT :: Ref Edit -> EditT a -> MusicBrainz (a, [Change])
runEditT editId action = do
  (a, changes) <- fmap runWriterT (nestMb action) >>= liftIO
  mapM_ linkChange changes
  return (a, changes)

  where

    linkChange :: Change -> MusicBrainz ()
    linkChange (Change r) = linkRevisionToEdit editId r


--------------------------------------------------------------------------------
runUpdate
  :: (Editable a, NewEntityRevision a, RealiseTree a, ViewRevision a)
  => Ref Editor -> Ref (Revision a) -> Tree a -> EditT (Ref (Revision a))
runUpdate editor base tree = do
  treeId <- realiseTree tree
  revisionId <- newChildRevision editor base treeId
  includeRevision revisionId
  return revisionId


--------------------------------------------------------------------------------
{-| Merge one entity into another. -}
merge :: (FindLatest a, Editable a, CloneRevision a)
  => Ref Editor -> Ref (Revision a) -> Ref a -> EditT (Ref (Revision a))
merge editor baseRev targetId = do
  -- Find the latest revision to merge into
  latestTarget <- viewOnce findLatest targetId
  mergeInto <- cloneRevision latestTarget editor

  -- Link this revision to both the old tree and the latest version,
  -- and include it in the edit.
  includeRevision mergeInto
  addChild mergeInto baseRev
  addChild mergeInto (coreRevision latestTarget)

  return mergeInto


--------------------------------------------------------------------------------
{-| Create an edit, and run an 'EditT' action to create the various components
of the edit (that is, link revisions to the edit). -}
createEdit :: EditT a -> MusicBrainz (Ref Edit)
createEdit actions = do
  editId <- openEdit
  runEditT editId actions
  return editId

--------------------------------------------------------------------------------
{-| Allow an editor to cast a 'Vote' on an edit. -}
voteOnEdit :: Ref Editor -> Ref Edit -> VoteScore -> MusicBrainz ()
voteOnEdit editorId editId vote = void $ execute
  [sql| INSERT INTO vote (edit_id, editor_id, vote) VALUES (?, ?, ?) |]
    (editId, editorId, vote)


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
{-| A vote on an edit. -}
data Vote = Vote { voteVote :: !VoteScore
                 , voteEditor :: !(Ref Editor)
                 , voteSuperceded :: !Bool
                 }
  deriving (Eq, Show)

instance FromRow Vote where
  fromRow = Vote <$> field <*> field <*> field


--------------------------------------------------------------------------------
{-| The possible types of votes that editors can cast on an edit. -}
data VoteScore = Accept | Reject | Abstain
  deriving (Eq, Show)

-- A custom instance here allows us to use -1 for reject.
instance Enum VoteScore where
  fromEnum Accept = 1
  fromEnum Reject = -1
  fromEnum Abstain = 0

  toEnum 1 = Accept
  toEnum (-1) = Reject
  toEnum 0 = Abstain
  toEnum n = error $ show n ++ " cannot be converted to Vote"

  enumFrom = boundedEnumFrom

instance Bounded VoteScore where
  minBound = Reject
  maxBound = Accept

instance FromField VoteScore where
  fromField f v = toEnum <$> fromField f v

instance ToField VoteScore where
  toField = toField . fromEnum


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
