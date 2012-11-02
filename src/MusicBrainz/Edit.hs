{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
module MusicBrainz.Edit
    ( Edit(..)
    , Change(..)
    , Editable(..)
    , Ref(..)
    , EditNote(..)
    ) where

import Data.Text (Text)

import MusicBrainz.Monad
import MusicBrainz.Types

--------------------------------------------------------------------------------
{-| An edit bundles up multiple 'Revision's that have not yet been applied to
entities. Editors can then vote on these edits to decide if they should be
merge, which ModBot can then later merge (or reject) once a consensus
emerges. -}
data Edit = Edit { editChanges :: [Change] }

data instance Ref Edit = EditRef Int

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
  includeRevision :: Ref Edit -> Ref (Revision a) -> MusicBrainz ()

  {-| Merge a revision on top of the current master revision. -}
  mergeRevisionUpstream :: Ref (Revision a) -> MusicBrainz ()


--------------------------------------------------------------------------------
{-| An edit note is a comment that can be left by editors on edit notes, to
have a discussion about the changes being made, or to provide references for
other editors to verify changes against. -}
data EditNote = EditNote
  { editNoteBody :: Text
  , editNoteAuthor :: Ref Editor
  }

data instance Ref EditNote = EditNoteRef
