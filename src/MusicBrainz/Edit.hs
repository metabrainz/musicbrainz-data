{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-| Data types and type classes related to MusicBrainz edits, and general
editing in MusicBrainz. -}
module MusicBrainz.Edit
    ( Edit(..)
    , Change(..)
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

import Control.Monad.Trans
import Control.Monad.Trans.Writer

import MusicBrainz.Data.FindLatest
import MusicBrainz.Data.Tree (ViewTree)
import MusicBrainz.Merge
import MusicBrainz.Monad
import MusicBrainz.Types

--------------------------------------------------------------------------------
{-| An existential wrapper around 'Ref' 'Revision'. Essentially, a 'Change' is
a 'Ref' 'Revision' except /you don't know what type/ @a@ is. All you know is
that it's an instance of 'Editable'.

This may seem confusing, but this is a trick to work with the fact that lists
are homogeneous in Haskell. However, a list of changes inside an edit is
heterogenous - a user may have changed artists, labels and releases within
a single edit, for example.

For more information on this technique, have a read of:

    * <http://www.haskell.org/haskellwiki/Existential_type>

    * <http://www.haskell.org/haskellwiki/Heterogenous_collections>

-}
data Change = forall a. Editable a => Change (Ref (Revision a))


--------------------------------------------------------------------------------
{-| The 'Editable' class has instances which have versioning and thus can be
included in edits. -}
class (FindLatest a, MasterRevision a, Mergeable (Tree a), NewEntityRevision a, RealiseTree a, ViewRevision a, ViewTree a) => Editable a where
  {-| Add a revision into an edit. -}
  linkRevisionToEdit :: Ref Edit -> Ref (Revision a) -> MusicBrainz ()


--------------------------------------------------------------------------------
{-| Accumulate many changes inside a single Edit. -}
type EditM = MusicBrainzT (WriterT [Change] IO)


--------------------------------------------------------------------------------
{-| Include a specific 'Revision' as part of an edit.

This is a fairly low-level operation, and you should be careful that you only
include revisions that haven't already been merged! -}
includeRevision :: (Editable a)
  => Ref (Revision a) -> EditM ()
includeRevision = lift . tell . return . Change


--------------------------------------------------------------------------------
class NewEntityRevision a where
  newEntityRevision :: (Functor m, MonadIO m)
    => Ref (Revision a) -> Ref a -> Ref (Tree a) -> MusicBrainzT m ()


--------------------------------------------------------------------------------
class RealiseTree a where
  realiseTree :: (Functor m, MonadIO m)
    => Tree a -> MusicBrainzT m (Ref (Tree a))


--------------------------------------------------------------------------------
class MasterRevision a where
  setMasterRevision :: (Functor m, MonadIO m)
    => Ref a -> Ref (Revision a) -> MusicBrainzT m ()


--------------------------------------------------------------------------------
{-| View a specific revision, along with the basic 'treeData'. -}
class ViewRevision a where
  viewRevision :: (Functor m, MonadIO m)
    => Ref (Revision a) -> MusicBrainzT m (CoreEntity a)
