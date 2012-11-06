{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-| Data types and type classes related to MusicBrainz edits, and general
editing in MusicBrainz. -}
module MusicBrainz.Edit
    ( Edit(..)
    , Change(..)
    , Editable(..)
    , Ref(..)
    , EditNote(..)
    , EditStatus(..)
    , Vote(..)
    , EditM
    , includeRevision
    ) where

import Control.Monad.Trans
import Control.Monad.Trans.Writer

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
class Editable a where
  {-| Add a revision into an edit. -}
  linkRevisionToEdit :: Ref Edit -> Ref (Revision a) -> MusicBrainz ()

  {-| Merge a revision on top of the current master revision. -}
  mergeRevisionUpstream :: Ref (Revision a) -> MusicBrainz ()


--------------------------------------------------------------------------------
{-| Accumulate many changes inside a single Edit. -}
type EditM = MusicBrainzT (WriterT [Change] IO)


--------------------------------------------------------------------------------
includeRevision :: Editable a => Ref (Revision a) -> EditM ()
includeRevision = lift . tell . return . Change
