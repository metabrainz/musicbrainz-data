{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module MusicBrainz.Versioning where

import Control.Applicative
import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer (WriterT(..), tell)
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Data.Tagged
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (In(..), Only(..), (:.)(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.ToRow (ToRow(..))
import GHC.Enum (boundedEnumFrom)

import qualified Data.Map as Map
import qualified Data.Set as Set

import MusicBrainz.Class.RootTable
import MusicBrainz.MBID
import MusicBrainz.Merge
import MusicBrainz.Monad
import MusicBrainz.Util (viewOnce)


--------------------------------------------------------------------------------
{-| A reference to a specific entity. In the database, this a foreign key
relationship to an entity of type @a@. -}
data Ref a = Referenceable a => Ref !(RefSpec a)

deriving instance Eq (Ref a)
deriving instance Ord (Ref a)
deriving instance Show (Ref a)

{-| The family of types which can be referenced via a primary key. -}
class (Eq (RefSpec a), Ord (RefSpec a), Show (RefSpec a)) => Referenceable a where
  {-| The exact type of all attributes that make up a reference. For example,
  a PostgreSQL @SERIAL@ field would be 'Int', while a compound key might be
  @(@'Int'@, @'Int'). -}
  type RefSpec a :: *


--------------------------------------------------------------------------------
{-| Unpack a reference into its individual attributes. -}
dereference :: Referenceable a => Ref a -> RefSpec a
dereference  = view (from reference)


--------------------------------------------------------------------------------
{-| An 'Iso'morphism to move between a set of attributes and a reference, and
back again. -}
reference :: Referenceable a => Iso' (RefSpec a) (Ref a)
reference = iso Ref (\(Ref r) -> r)


--------------------------------------------------------------------------------
{-| An 'Entity' is something that has been loaded from the database. It cotains
both data about itself (in @entityData@), and also a reference to itself (in
@entityRef@) so that other data/entities can refer to it. -}
data Entity a = Entity { entityRef :: !(Ref a)
                       , entityData :: !a
                       }

deriving instance (Eq a, Show a) => Eq (Entity a)
deriving instance (Eq a, Show a) => Show (Entity a)

instance (FromField (Ref a), FromRow a) => FromRow (Entity a) where
  fromRow = Entity     -- Entity reference
                   <$> field
                       -- Delegetate to the actual entity to parse its data.
                   <*> fromRow


--------------------------------------------------------------------------------
{-| Represents a view of a versioned MusicBrainz \'core\' entity at a specific
point in time (a specific 'Revision'). -}
data CoreEntity a = CoreEntity
    { coreRef :: !(Ref a)
    , coreRevision :: !(Ref (Revision a))
    , coreData :: !a
    }

deriving instance (Eq a, Show a) => Eq (CoreEntity a)
deriving instance (Eq a, Show a) => Show (CoreEntity a)

instance (FromField (Ref a), FromRow a) => FromRow (CoreEntity a) where
  fromRow = CoreEntity     -- Core entity's MBID
                       <$> field
                           -- The revision reference
                       <*> field
                           -- Delegetate to the actual entity to parse its data.
                       <*> fromRow


--------------------------------------------------------------------------------
{-| The 'Add' type class allows you to add new entities that are not
versioned. -}
class Add a where
  {-| Add a new entity, with some starting data, producing a fresh 'Entity'
  with a 'Ref'. -}
  add :: (Functor m, MonadIO m) => a -> MusicBrainzT m (Entity a)


--------------------------------------------------------------------------------
{-| A revision is a version of an entity at a specific point in time. The type
@a@ indicates what type of entity this is a revision of (e.g., @Revision Artist@
means a specific revision of an 'Artist'). -}
data Revision a = Revision { revisionCreatedAt :: !UTCTime }
  deriving (Eq, Show)

instance Referenceable (Revision a) where
  type RefSpec (Revision a) = Int

instance FromField (Ref (Revision a)) where
  fromField f v = view reference <$> fromField f v

instance FromRow (Revision a) where
  fromRow = Revision <$> field

instance ToField (Ref (Revision a)) where
  toField = toField . dereference


--------------------------------------------------------------------------------
{-| Attempt to resolve a the revision which 2 revisions forked from. -}
mergeBase :: (Functor m, MonadIO m) => Ref (Revision a) -> Ref (Revision a)
          -> MusicBrainzT m (Maybe (Ref (Revision a)))
mergeBase a b = listToMaybe . map fromOnly <$> query
  [sql| WITH RECURSIVE revision_path (revision_id, parent_revision_id, distance)
        AS (
          SELECT revision_id, parent_revision_id, 1
          FROM revision_parent
          WHERE revision_id IN ?

          UNION

          SELECT
            revision_path.revision_id, revision_parent.parent_revision_id,
            distance + 1
          FROM revision_parent
          JOIN revision_path
            ON (revision_parent.revision_id = revision_path.parent_revision_id)
        )
        SELECT parent_revision_id
        FROM revision_path a
        JOIN revision_path b USING (parent_revision_id)
        ORDER BY a.distance, b.distance
        LIMIT 1 |] (Only $ In [a, b])



--------------------------------------------------------------------------------
{-| Find references to the parent revisions of a given revision. -}
revisionParents :: (Functor m, MonadIO m)
  => Ref (Revision a) -> MusicBrainzT m (Set.Set (Ref (Revision a)))
revisionParents r =
  Set.fromList . map fromOnly <$> query q (Only r)
  where q = [sql| SELECT parent_revision_id FROM revision_parent
                  WHERE revision_id = ? |]


--------------------------------------------------------------------------------
revisionChildren :: (Functor m, MonadIO m)
  => Ref (Revision a) -> MusicBrainzT m (Set.Set (Ref (Revision a)))
revisionChildren r =
  Set.fromList . map fromOnly <$> query q (Only r)
  where q = [sql| SELECT revision_id FROM revision_parent
                  WHERE parent_revision_id = ? |]


--------------------------------------------------------------------------------
instance GetEntity (Revision a) where
  getEntity r = head <$>
    query [sql| SELECT revision_id, created_at
                FROM revision
                WHERE revision_id = ? |] (Only r)


--------------------------------------------------------------------------------
{-| View a specific revision, along with the basic 'treeData'. -}
class ViewRevision a where
  viewRevision :: (Functor m, MonadIO m)
    => Ref (Revision a) -> MusicBrainzT m (CoreEntity a)


--------------------------------------------------------------------------------
class MasterRevision a where
  setMasterRevision :: (Functor m, MonadIO m)
    => Ref a -> Ref (Revision a) -> MusicBrainzT m ()

  default setMasterRevision
    :: (Functor m, MonadIO m, RootTable a, ToField (Ref a), ToField (Ref (Revision a)))
    => Ref a -> Ref (Revision a) -> MusicBrainzT m ()
  setMasterRevision entityId revisionId = void $
    execute q (revisionId, entityId)
   where
    table = untag (rootTable :: Tagged a String)
    q = fromString $ unlines
      [ "UPDATE " ++ table ++ " SET master_revision_id = ? "
      , "WHERE " ++ table ++ "_id = ?"
      ]


--------------------------------------------------------------------------------
class NewEntityRevision a where
  newEntityRevision :: (Functor m, MonadIO m)
    => Ref (Revision a) -> Ref a -> Ref (Tree a) -> MusicBrainzT m ()

  default newEntityRevision
    :: (Functor m, MonadIO m, RootTable a
       , ToField (Ref (Tree a)), ToField (Ref (Revision a)), ToField (Ref a))
    => Ref (Revision a) -> Ref a -> Ref (Tree a) -> MusicBrainzT m ()
  newEntityRevision revisionId entityId entityTreeId = void $
    execute q (entityId, revisionId, entityTreeId)
   where
    entityName = untag (rootTable :: Tagged a String)
    q = fromString $ unlines
          [ "INSERT INTO " ++ entityName ++ "_revision (" ++ entityName ++ "_id, revision_id, " ++ entityName ++ "_tree_id) "
          , "VALUES (?, ?, ?)"
          ]


--------------------------------------------------------------------------------
{-| The create type class allows you to create new entities. -}
class Create a where
  {-| Create a new entity, with some starting data, producing a fresh MBID. -}
  create :: Ref Editor -> Tree a -> EditT (Ref (Revision a))

  default create
    :: ( Editable a, FromField (Ref a), MasterRevision a, NewEntityRevision a
       , RealiseTree a, RootTable a
       )
    => Ref Editor -> Tree a -> EditT (Ref (Revision a))
  create editor entity = do
    treeId <- realiseTree entity
    entityId <- reserveEntityTable (untag (rootTable :: Tagged a String))
    revisionId <- newUnlinkedRevision editor
    newEntityRevision revisionId entityId treeId
    setMasterRevision entityId revisionId
    includeRevision revisionId
    return revisionId

   where

    reserveEntityTable table = selectValue $ query_ $
      fromString ("INSERT INTO " ++ table ++ " (master_revision_id) VALUES (-1) RETURNING " ++ table  ++ "_id")


--------------------------------------------------------------------------------
class GetEntity a where
  getEntity :: (Functor m, MonadIO m) => Ref a -> MusicBrainzT m (Entity a)

--------------------------------------------------------------------------------
{-| Trees for entities are a somewhat internal concept of the way MusicBrainz
versioning works. A tree consists of all the data that is versioned for a
specific entity (of type @a@). -}
class HasTree a where
  data Tree a :: *

  {-| A convenience accessor to the \'essential\' data inside a tree (the data
  which contains the entities, and so on.) -}
  treeData :: Tree a -> a

instance Referenceable (Tree a) where
  type RefSpec (Tree a) = Int

instance FromField (Ref (Tree a)) where
  fromField f v = view reference <$> fromField f v

instance ToField (Ref (Tree a)) where
  toField = toField . dereference


--------------------------------------------------------------------------------
{-| View all data about a specific version of an entity. -}
class ViewTree a where
  viewTree :: (Applicative m, MonadIO m)
    => Ref (Revision a) -> MusicBrainzT m (Tree a)


--------------------------------------------------------------------------------
{-| A MusicBrainz editor who makes changes to the database. -}
data Editor = Editor { editorName :: !Text
                     , editorPassword :: !Text
                     }
  deriving (Eq, Show)

instance Referenceable Editor where
  type RefSpec Editor = Int

instance FromField (Ref Editor) where
  fromField f v = view reference <$> fromField f v

instance FromRow Editor where
  fromRow = Editor <$> field <*> field

instance ToField (Ref Editor) where
  toField = toField . dereference

instance ToRow Editor where
  toRow Editor{..} = [ toField editorName
                     , toField editorPassword
                     ]

instance ResolveReference Editor where
  resolveReference editorId = listToMaybe . map fromOnly <$>
    query [sql| SELECT id FROM editor WHERE id = ? |] (Only editorId)


--------------------------------------------------------------------------------
{-| Look up an editor by their name. -}
findEditorByName :: Text -> MusicBrainz (Maybe (Entity Editor))
findEditorByName name =
  listToMaybe <$> query [sql| SELECT id, name, password FROM editor WHERE name = ? |]
             (Only name)


--------------------------------------------------------------------------------
{-| Register a new MusicBrainz editor. -}
register :: Editor -> MusicBrainz (Entity Editor)
register editor = head <$> query
  [sql| INSERT INTO editor (name, password) VALUES (?, ?)
        RETURNING id, name, password |] editor


--------------------------------------------------------------------------------
class ResolveReference a where
  {-| Attempt to resolve a reference from its attributes. If the attributes
  don't actually correspond to an entity in the database, then 'Nothing' is
  returned. -}
  resolveReference
    :: (Functor m, MonadIO m) => RefSpec a -> MusicBrainzT m (Maybe (Ref a))

  default resolveReference
    :: (Functor m, MonadIO m, GenericResolver a (RefSpec a))
    => RefSpec a -> MusicBrainzT m (Maybe (Ref a))
  resolveReference = genericResolveReference


--------------------------------------------------------------------------------
class RefSpec a ~ r => GenericResolver a r | r -> a where
  genericResolveReference
    :: (Functor m, MonadIO m)
    => r -> MusicBrainzT m (Maybe (Ref a))

instance (RootTable a, FromField (Ref a), RefSpec a ~ MBID a) => GenericResolver a (MBID a) where
  genericResolveReference entityMbid =
    listToMaybe . map fromOnly <$> query q (Only entityMbid)
   where
    eName = untag (rootTable :: Tagged a String)
    q = fromString $ unlines
        [ "WITH RECURSIVE path (revision_id, " ++ eName ++ "_id, child_revision_id, created_at, is_master_revision_id) "
        , "AS ("
        , "  SELECT "
        , "    " ++ eName ++ "_revision.revision_id, "
        , "    " ++ eName ++ "_revision." ++ eName ++ "_id, "
        , "    revision_parent.revision_id AS child_revision_id, "
        , "    created_at, "
        , "    TRUE as is_master_revision_id "
        , "  FROM " ++ eName ++ "_revision "
        , "  JOIN " ++ eName ++ " USING (" ++ eName ++ "_id) "
        , "  JOIN revision USING (revision_id) "
        , "  LEFT JOIN revision_parent ON (revision_parent.parent_revision_id = revision.revision_id) "
        , "  WHERE " ++ eName ++ "_id = ? AND master_revision_id = " ++ eName ++ "_revision.revision_id "
        , " "
        , "  UNION "
        , " "
        , "  SELECT "
        , "    " ++ eName ++ "_revision.revision_id, "
        , "    " ++ eName ++ "_revision." ++ eName ++ "_id, "
        , "    revision_parent.revision_id, "
        , "    revision.created_at, "
        , "    master_revision_id = " ++ eName ++ "_revision.revision_id AS is_master_revision_id "
        , "  FROM path "
        , "  JOIN " ++ eName ++ "_revision ON (path.child_revision_id = " ++ eName ++ "_revision.revision_id) "
        , "  JOIN revision ON (revision.revision_id = " ++ eName ++ "_revision.revision_id) "
        , "  JOIN " ++ eName ++ " ON (" ++ eName ++ "." ++ eName ++ "_id = " ++ eName ++ "_revision." ++ eName ++ "_id) "
        , "  LEFT JOIN revision_parent ON (revision_parent.parent_revision_id = " ++ eName ++ "_revision.revision_id) "
        , ") "
        , "SELECT " ++ eName ++ "_id "
        , "FROM path "
        , "WHERE is_master_revision_id "
        , "ORDER BY created_at DESC, revision_id DESC "
        , "LIMIT 1 "
        ]

instance RootTable a => GenericResolver (Revision a) Int where
  genericResolveReference revisionId =
    listToMaybe . map fromOnly <$> query q (Only revisionId)
   where
    eName = untag (rootTable :: Tagged a String)
    q = fromString $ unlines
        [ "SELECT revision_id "
        , "FROM " ++ eName ++ "_revision "
        , "WHERE revision_id = ?"
        ]


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


--------------------------------------------------------------------------------
{-| Create a new \'system\' revision, that is not yet bound to any entity. -}
newUnlinkedRevision :: (Functor m, MonadIO m)
  => Ref Editor -> MusicBrainzT m (Ref (Revision a))
newUnlinkedRevision editor = selectValue $
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

  default cloneRevision
    :: (Functor m, MonadIO m, ToField (Ref a), RootTable a)
    => CoreEntity a -> Ref Editor -> MusicBrainzT m (Ref (Revision a))
  cloneRevision a editor = do
    revId <- newUnlinkedRevision editor
    selectValue $ query q (coreRef a, revId, coreRevision a)

   where

      entityName = untag (rootTable :: Tagged a String)
      q = fromString $ unlines
          [ "INSERT INTO " ++ entityName ++ "_revision (" ++ entityName ++ "_id, revision_id, " ++ entityName ++ "_tree_id) "
          , "VALUES (?, ?, (SELECT " ++ entityName ++ "_tree_id FROM " ++ entityName ++ "_revision WHERE revision_id = ?)) "
          , "RETURNING revision_id"
          ]



--------------------------------------------------------------------------------
newChildRevision :: (Functor m, MonadIO m, ViewRevision a, NewEntityRevision a)
  => Ref Editor -> Ref (Revision a) -> Ref (Tree a)
  -> MusicBrainzT m (Ref (Revision a))
newChildRevision editorId baseRevisionId treeId = do
  entity <- viewRevision baseRevisionId
  revisionId <- newUnlinkedRevision editorId
  newEntityRevision revisionId (coreRef entity) treeId
  addChild revisionId baseRevisionId
  return revisionId


--------------------------------------------------------------------------------
class RealiseTree a where
  realiseTree :: (Functor m, MonadIO m)
    => Tree a -> MusicBrainzT m (Ref (Tree a))

--------------------------------------------------------------------------------
{-| Attempt to find the latest revision of an entity (type @a@), by a given
'Ref'. To obtain the reference, you can use
'MusicBrainz.Merge.resolveMbid'. -}
class FindLatest a where
  findLatest :: (Applicative m, Functor m, MonadIO m) =>
    Set.Set (Ref a) -> MusicBrainzT m (Map.Map (Ref a) (CoreEntity a))
