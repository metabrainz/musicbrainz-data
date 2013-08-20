{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-| Data types and type classes related to MusicBrainz edits, and general
editing in MusicBrainz. -}
module MusicBrainz.Edit
    ( Edit(..)
    , Change(..)
    , CoreEntityRef(..)
    , Editable(..)
    , EditNote(..)
    , EditStatus(..)
    , Vote(..)
    , EditM
    , includeRevision
    , NewEntityRevision(..)
    , RealiseTree(..)
    , MasterRevision(..)
    , ViewRevision(..)
    ) where

import Control.Lens
import Control.Monad (void)
import Control.Monad.Trans
import Control.Monad.Trans.Writer
import Database.PostgreSQL.Simple.ToField (ToField)
import Data.String (fromString)
import Data.Tagged (Tagged, untag)

import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.CoreEntity
import MusicBrainz.Data.Tree (ViewTree)
import MusicBrainz.Merge
import MusicBrainz.Monad
import MusicBrainz.Types

--------------------------------------------------------------------------------
data Change = ArtistChange !(Ref (Revision Artist))
            | LabelChange !(Ref (Revision Label))
            | RecordingChange !(Ref (Revision Recording))
            | ReleaseChange !(Ref (Revision Release))
            | ReleaseGroupChange !(Ref (Revision ReleaseGroup))
            | UrlChange !(Ref (Revision Url))
            | WorkChange !(Ref (Revision Work))


--------------------------------------------------------------------------------
data CoreEntityRef = ArtistRef !(Ref Artist)
                   | LabelRef !(Ref Label)
                   | RecordingRef !(Ref Recording)
                   | ReleaseRef !(Ref Release)
                   | ReleaseGroupRef !(Ref ReleaseGroup)
                   | UrlRef !(Ref Url)
                   | WorkRef !(Ref Work)


--------------------------------------------------------------------------------
{-| The 'Editable' class has instances which have versioning and thus can be
included in edits. -}
class (FindLatest a, MasterRevision a, Mergeable (Tree a), NewEntityRevision a, RealiseTree a, ViewRevision a, ViewTree a) => Editable a where
  {-| Add a revision into an edit. -}
  linkRevisionToEdit :: Ref Edit -> Ref (Revision a) -> MusicBrainz ()

  default linkRevisionToEdit
    :: (CoreEntityTable a, ToField (Ref Edit), ToField (Ref (Revision a)))
    => Ref Edit -> Ref (Revision a) -> MusicBrainz ()
  linkRevisionToEdit editId revisionId = void $ execute q (editId, revisionId)
   where
    table = "edit_" ++ untag (rootTable :: Tagged a String)
    q = fromString $ unlines
          [ "INSERT INTO " ++ table ++ " (edit_id, revision_id)"
          , " VALUES (?, ?)"
          ]

  change :: Prism' Change (Ref (Revision a))


--------------------------------------------------------------------------------
{-| Accumulate many changes inside a single Edit. -}
type EditM = MusicBrainzT (WriterT [Change] IO)


--------------------------------------------------------------------------------
{-| Include a specific 'Revision' as part of an edit.

This is a fairly low-level operation, and you should be careful that you only
include revisions that haven't already been merged! -}
includeRevision :: Editable a => Ref (Revision a) -> EditM ()
includeRevision = lift . tell . return . review change


--------------------------------------------------------------------------------
class NewEntityRevision a where
  newEntityRevision :: (Functor m, MonadIO m)
    => Ref (Revision a) -> Ref a -> Ref (Tree a) -> MusicBrainzT m ()

  default newEntityRevision
    :: (Functor m, MonadIO m, CoreEntityTable a
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
class RealiseTree a where
  realiseTree :: (Functor m, MonadIO m)
    => Tree a -> MusicBrainzT m (Ref (Tree a))


--------------------------------------------------------------------------------
class MasterRevision a where
  setMasterRevision :: (Functor m, MonadIO m)
    => Ref a -> Ref (Revision a) -> MusicBrainzT m ()

  default setMasterRevision
    :: (Functor m, MonadIO m, CoreEntityTable a, ToField (Ref a), ToField (Ref (Revision a)))
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
{-| View a specific revision, along with the basic 'treeData'. -}
class ViewRevision a where
  viewRevision :: (Functor m, MonadIO m)
    => Ref (Revision a) -> MusicBrainzT m (CoreEntity a)
